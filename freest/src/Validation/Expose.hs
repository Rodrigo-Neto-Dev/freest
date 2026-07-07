module Validation.Expose
  ( kindArrow
  , function
  , arrow
  , externalChoice
  , internalChoice
  , output
  , input
  , affineOutput
  , affineInput
  , affineWait
  , typeOutput
  , typeInput
  , wait
  , canonicaliseProtocol
  )
where

import UI.Error
import Validation.Base
import Syntax.Base
import Syntax.Expression qualified as E
import Syntax.Kind qualified as K
import Syntax.Declarations qualified as D
import Syntax.Type.Kinded qualified as T
import Validation.Normalisation ( normalise, isWhnf, normWith, tNameRedex )

import Data.Functor
import Data.Bifunctor
import Data.Map qualified as Map
import Data.Set qualified as Set
import Control.Applicative
import Control.Monad.Trans.Except
import Control.Monad.State ( get, gets )

kindArrow :: K.Kind -> ([K.Kind], K.Kind)
kindArrow (K.Arrow _ k1 k2) = first (k1:) (kindArrow k2)
kindArrow k = ([], k)

function :: D.KindedTypeDecls -> E.KindedExp -> T.KindedType -> Validation T.KindedType
function tdecls e t = do
  case normalise tdecls t of
    t'@(T.AppArrow s m u v) -> pure t'
    t'@(T.AppForall s m aks u) -> pure t'
    _ -> throwE (ExposeError (getSpan e) (Right e) "a function" t)

arrow :: D.KindedTypeDecls -> E.KindedExp -> T.KindedType -> Validation (K.Multiplicity, T.KindedType, T.KindedType)
arrow tdecls e t = do
  case normalise tdecls t of
    t'@(T.AppArrow s m u v) -> pure (m, u, v)
    _ -> throwE (ExposeError (getSpan e) (Right e) "a monomorphic function" t)

exists :: D.KindedTypeDecls
       -> Either E.Pat E.KindedExp
       -> T.KindedType 
       -> Validation ([(Variable, K.Kind)], T.KindedType)
exists tdecls pe t = do
  case normalise tdecls t of
    t'@(T.AppExists s aks u) -> pure (aks, u)
    _ -> throwE (TypeMismatchExists (getSpan pe) t pe) 

externalChoice :: D.KindedTypeDecls -> E.Pat -> T.KindedType -> Identifier -> Validation T.KindedType
externalChoice tdecls p t i = do
  case normalise tdecls t of
    T.AppLinChoice _ T.In lts -> case lookup i lts of
      Just ti -> return ti
      Nothing -> throwE (IllegalChoice (getSpan i) i t)
    t'@(T.UnChoice _ T.In ls)
      | i `elem` ls -> return t'
      | otherwise   -> throwE (IllegalChoice (getSpan i) i t)
    (T.AppSemi _ t'@(T.UnChoice _ T.In ls) u)
      | i `elem` ls -> return t'
      | otherwise   -> throwE (IllegalChoice (getSpan i) i t)
    _ -> throwE (ExposeError (getSpan p) (Left p) "an external choice channel" t)

internalChoice :: D.KindedTypeDecls -> E.KindedExp -> T.KindedType -> Identifier -> Validation T.KindedType
internalChoice tdecls e t i = do
  case normalise tdecls t of
    T.AppLinChoice s T.Out its -> 
      case lookup i its of
        Just t' -> return t'
        Nothing -> throwE (IllegalChoice s i t)
    t'@(T.UnChoice s T.Out its)
      | i `elem` its -> return t'
      | otherwise    -> throwE (IllegalChoice s i t)
    _ -> throwE (ExposeError (getSpan e) (Right e) "an internal choice channel" t)

output :: D.KindedTypeDecls -> E.KindedExp -> T.KindedType -> Validation (T.KindedType, T.KindedType)
output tdecls = message tdecls T.Out . Right

input :: D.KindedTypeDecls -> Either E.Pat E.KindedExp -> T.KindedType -> Validation (T.KindedType, T.KindedType)
input tdecls = message tdecls T.In

-- | Expose the head step of the protocol wrapped in an affine sender capability.
-- The wrapper always carries a canonical session protocol (see 'message' for the
-- boundary at which continuations are canonicalised): we unwrap and step; we do
-- NOT re-normalise here because the protocol inside the wrapper is canonical
-- by construction (see 'Typing.synthAffineBuiltin.newA' and 'message').
affineOutput :: D.KindedTypeDecls -> E.KindedExp -> T.KindedType -> Validation (T.KindedType, T.KindedType)
affineOutput tdecls e t = do
  case normalise tdecls t of
    T.AffineSender s proto -> do
      (payload, cont) <- output tdecls e proto
      pure (payload, T.AffineSender s cont)
    _ -> throwE (ExposeError (getSpan e) (Right e) "an affine output channel" t)

-- | Expose the head step of the protocol wrapped in an affine receiver capability.
-- The wrapper is canonical by construction (see 'message'); no defensive
-- re-normalisation is needed when rewrapping.
affineInput :: D.KindedTypeDecls -> Either E.Pat E.KindedExp -> T.KindedType -> Validation (T.KindedType, T.KindedType)
affineInput tdecls pe t = do
  case normalise tdecls t of
    T.AffineReceiver s proto -> do
      (payload, cont) <- input tdecls pe proto
      pure (payload, T.AffineReceiver s cont)
    _ -> throwE (ExposeError (getSpan pe) pe "an affine input channel" t)

-- | Consume an affine receiver capability whose protocol is at 'Wait'.
-- Delegates to 'wait' on the inner session protocol, so the capability
-- layer never has to inspect raw 'End' values.
affineWait :: D.KindedTypeDecls -> Either E.Pat E.KindedExp -> T.KindedType -> Validation ()
affineWait tdecls pe t = do
  case normalise tdecls t of
    T.AffineReceiver _ proto -> wait tdecls (E.WaitPat (getSpan pe)) proto
    _ -> throwE (ExposeError (getSpan pe) pe "an affine input channel" t)

message :: D.KindedTypeDecls -> T.Polarity -> Either E.Pat E.KindedExp -> T.KindedType
        -> Validation (T.KindedType, T.KindedType)
message tdecls p pe t = do
  case normalise tdecls t of
    -- Linear head message: the semicolon after it is consumed, so the
    -- continuation is the identity 'Skip' (already canonical).
    T.AppMessage s K.Lin{} p' u                    | p == p' -> return (u, T.Skip s)
    -- Unbounded head message without a semicolon: the protocol IS the
    -- message, which is already canonical — no continuation to canonicalise.
    t'@(T.AppMessage s K.Un{}  p' u)               | p == p' -> return (u, t')
    -- Sequential composition: peel off the head message and canonicalise the
    -- continuation before returning. 'normalise tdecls t' is only a WHNF;
    -- anything under the semicolon (e.g. 'Dual Close' at the tail of a
    -- '?a ; Close' protocol reduced via R-DSemi + R-SemiL) would otherwise
    -- leak out as '**?Dual Close' when rewrapped into an 'AffineReceiver'.
    -- 'canonicaliseProtocol' (from 'Validation.Expose') recurses through
    -- 'AppSemi', threading 'tNameRedex', and is the single seam that
    -- guarantees every continuation reaching an AffineSender/AffineReceiver
    -- is canonical.
    T.AppSemi _    (T.AppMessage _ K.Lin{} p' u) v | p == p' -> return (u, canonicaliseProtocol tdecls v)
    -- Unbounded message inside a ';' tail: the message itself is canonical
    -- (it is the head AND the continuation as e.g. '*!a'_); we still need
    -- to canonicalise the tail 'v' in case it carries 'Dual' or pending
    -- 'AppSemi' residuals.
    T.AppSemi _ t'@(T.AppMessage _ K.Un{}  p' u) v | p == p' -> return (u, canonicaliseProtocol tdecls v)
    _ -> throwE (ExposeError (getSpan pe) pe msg t)
  where msg = "an " ++ (case p of T.In -> "input"; T.Out -> "output") ++ " channel"

-- | Canonicalise a session protocol for storage inside an AffineSender /
-- AffineReceiver wrapper.
--
-- Two-level normalisation regime (intentional):
--
--   * 'Validation.Normalisation.normalise' is a WHNF function and STAYS
--     WHNF. Every other site in the compiler depends on it being WHNF (the
--     'isWhnf' clauses for 'AppSemi _ T.AppMessage{} _', 'End', 'Skip', etc.
--     are deliberate stopping points). Do not try to push normalisation
--     further inside 'Validation.Normalisation'.
--
--   * 'canonicaliseProtocol' goes one step further: it WHNFs to a head step,
--     sees whether the head is an 'AppSemi', and if so BOTH-sides recursively
--     canonicalises the protocol. This eliminates 'Dual' residuals that
--     ride inside semicolon tails (e.g. 'Dual Close' inside '?a ; Close')
--     which WHNF cannot reach. The recursion threads the µ-marker set via
--     'tNameRedex' + 'normWith' so that equi-recursive protocols remain
--     terminating — no extra cycle-detection logic is invented here.
--
-- INVARIANT maintained by the capability layer:
--
--   > @'T.AffineSender' span p@   and   @'T.AffineReceiver' span q@
--   >
--   > always wrap canonical session protocols (no 'T.AppDual', no
--   > partially-reduced 'T.AppSemi' with a residual at the tail, no
--   > 'T.AppTName'). Every wrapper-construction site AND every continuation
--   > returned by 'message' MUST go through this helper.
--
-- This function is invoked ONLY at protocol boundaries (the affine-wrapper
-- seam and the session→capability message boundary). It is intentionally
-- NOT used at non-affine type sites; the rest of the compiler relies on
-- 'normalise' being WHNF.
canonicaliseProtocol :: D.KindedTypeDecls -> T.KindedType -> T.KindedType
canonicaliseProtocol tdecls = go Set.empty
  where
    -- IMPORTANT cycle-detection ordering:
    --
    -- We call 'normWith' first with the UNMODIFIED 'visited'. 'normWith'
    -- itself inserts any 'tNameRedex t' it finds into the set it threads
    -- forward, and aborts a re-encounter to 'Void'. Pre-augmenting 'visited'
    -- here would defeat that machinery: the very first time we see a new µ
    -- type-name, calling 'normWith' with the marker already in the set
    -- would return 'Void' BEFORE it ever unfolds.
    --
    -- After 'normWith' returns a WHNF, we extend the INCOMING 'visited'
    -- with the marker of the ORIGINAL 't' (not the WHNF — the WHNF may be
    -- a different shape after reduction) and pass that to children. This
    -- means a child whose reduction re-enters the same µ-name will fail
    -- with 'Void' rather than diverging, while the top-level unfolding of
    -- a non-recursive µ-type still produces its unfolded body.
    --
    -- Equivalently: each 'normWith' invocation carries the cycle baggage of
    -- the FRAME (parent) over the LEAF work it does in the recursive call.
    go visited t =
      let whnf      = normWith tdecls visited t
          visited'  = case tNameRedex t of
            Just u  -> Set.insert u visited
            Nothing -> visited
      in case whnf of
        -- Sequential composition: must canonicalise BOTH sides while the
        -- WHNF only inspects the head. The outer µ-marker (if any) is
        -- inherited by the children via 'visited''.
        T.AppSemi s t1 t2 -> T.AppSemi s (go visited' t1) (go visited' t2)
        -- Linear choice WHNF (e.g. having come out of R-SChoiceDist on the
        -- left of an 'AppSemi'): the label bodies still must be canonicalised.
        -- Without this, 'Dual' from a chosen label would surface later.
        T.AppLinChoice s p lts ->
          T.AppLinChoice s p (map (second (go visited')) lts)
        -- Higher-order session type: the body may carry any protocol
        -- residual. Without descending, 'Dual' inside a type abstraction
        -- would leak through the cap-layer.
        T.AppQuantS s p a k t' ->
          T.AppQuantS s p a k (go visited' t')
        -- WHNF reached and it is none of the above: Done.
        -- (Covers 'T.AppMessage', 'T.UnChoice', 'T.End', 'T.AppVar',
        -- 'T.Skip', 'T.Void', and 'T.AffineSender'/'Receiver' via the
        -- 'isWhnf' clauses.)
        t' -> t'

typeOutput :: D.KindedTypeDecls -> E.KindedExp -> T.KindedType 
           -> Validation (Variable, K.Kind, T.KindedType)
typeOutput tdecls = typeMsg tdecls T.Out . Right

typeInput :: D.KindedTypeDecls -> Either E.Pat E.KindedExp -> T.KindedType 
          -> Validation (Variable, K.Kind, T.KindedType)
typeInput tdecls = typeMsg tdecls T.In

typeMsg :: D.KindedTypeDecls -> T.Polarity -> Either E.Pat E.KindedExp -> T.KindedType
            -> Validation (Variable, K.Kind, T.KindedType)
typeMsg tdecls p pe t = do
  case normalise tdecls t of
    T.AppQuantS _ p' a k t' | p == p' -> return (a, k, t')
    _ -> throwE (ExposeError (getSpan pe) pe msg t)
  where msg = "a type-" ++ (case p of T.In -> "input"; T.Out -> "output") ++ " channel"

wait :: D.KindedTypeDecls -> E.Pat -> T.KindedType -> Validation ()
wait tdecls p t = do
  case normalise tdecls t of
    T.End _ T.In -> return ()
    T.AppSemi _ (T.End _ T.In) _ -> return ()
    _ -> throwE (ExposeError (getSpan p) (Left p) "a `Wait` channel" t)
