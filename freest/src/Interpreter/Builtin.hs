{- |
Module      :  Interpreter.Builtin
Copyright   :  © The FreeST Team
Maintainer  :  freest-lang@listas.ciencias.ulisboa.pt

The builtin functions exposed to FreeST programs, together with the marshalling
helpers (between Haskell and FreeST values) and the channel operations they and
the evaluator rely on. Value rendering for output goes through 'unparse'.
-}
module Interpreter.Builtin
  ( builtins
  , asString
  , fstToHsBool
  , hsToFstString
  , chan
  , receive
  , receiveLabel
  , send
  ) where

import qualified Control.Concurrent.Chan as C ( newChan, readChan, writeChan, dupChan )
import Data.Char ( chr, ord )
import Data.Functor ( ($>) )
import qualified Data.Map as Map
import Data.IORef ( newIORef, atomicModifyIORef' )
import GHC.Float ( Floating(log1mexp, log1p, expm1, log1pexp) )

import Interpreter.Value ( Value(..), ChannelEnd )
import Parser.Unparser ( unparse )

-- | Convert Haskell's True and False into FreeST's value representation
hsToFstBool :: Bool -> Value
hsToFstBool True = VCons "True" []
hsToFstBool False = VCons "False" []

-- | Extract True and False from FreeST's value representation
fstToHsBool :: Value -> Bool
fstToHsBool (VCons "True" []) = True
fstToHsBool (VCons "False" []) = False

-- | Build a FreeST string value from a Haskell 'String'.
hsToFstString :: String -> Value
hsToFstString = foldr (\c acc -> VCons "(::)" [VChar c, acc]) (VCons "[]" [])

-- | Extract a Haskell 'String' from a FreeST string value.
fstToHsString :: Value -> String
fstToHsString = \case
  VCons "[]"   []              -> ""
  VCons "(::)" [VChar c, rest] -> c : fstToHsString rest
  v                            -> error ("fstToHsString: not a string: " ++ show v)

-- | A FreeST string value as a Haskell 'String', if it is one (used for
-- pattern matching; an empty list stays a list, since "" and [] are
-- indistinguishable).
asString :: Value -> Maybe String
asString = \case
  VCons "(::)" [VChar c, rest] -> (c :) <$> go rest
  _                            -> Nothing
  where
    go = \case
      VCons "[]"   []              -> Just ""
      VCons "(::)" [VChar d, more] -> (d :) <$> go more
      _                            -> Nothing

chan :: IO (ChannelEnd, ChannelEnd)
chan = do
  c1 <- C.newChan
  c2 <- C.newChan
  return ((c1, c2), (c2, c1))

receive :: ChannelEnd -> IO (Value, ChannelEnd)
receive c = do
  v <- C.readChan (fst c)
  return (v, c)

receiveLabel :: ChannelEnd -> IO (String, ChannelEnd)
receiveLabel c = do
  (VLabel s, _) <- receive c
  return (s, c)

send :: Value -> ChannelEnd -> IO ChannelEnd
send v c = do
  C.writeChan (snd c) v
  return c

sendLabel :: String -> ChannelEnd -> IO ChannelEnd
sendLabel s c = do
  send (VLabel s) c
  return c

wait :: Value -> Value
wait (VChan c) =
  VIO $ C.readChan (fst c)

close :: Value -> IO Value
close (VChan c) = do
  C.writeChan (snd c) VUnit
  return VUnit

-- * Create a new affine channel: a plain receiver end and a sender end
-- carrying a shared, atomically-updated count of live senders (starts at 1).
affineChan :: IO (Value, Value)
affineChan = do
  (chanL, chanR) <- chan
  ref <- newIORef 1
  return (VChan chanL, VAffineSender chanR ref)

-- affineChan returns (Value, Value), but VIO wraps IO Value
-- a single value, not a pair of values directly returned by IO.
-- This wraps it as a tuple value:
affineChan' :: IO Value
affineChan' = do
  (rx, wx) <- affineChan
  return $ VCons "(,)" [rx, wx]

-- sendA: write `Just x` on the wire. The sender is not consumed at runtime
-- (only the type system threads it linearly); the same value is returned.
sendA :: Value -> Value -> IO Value
sendA x chan = do
  let c = case chan of
            VAffineSender c' _ -> c'
            _ -> error $ "sendA: not an affine sender, got: " ++ show chan
  _ <- send (VCons "Just" [x]) c
  return chan

-- cloneAS: atomically increment the shared count; both results share the
-- channel end and the counter.
cloneSender :: Value -> IO Value
cloneSender (VAffineSender c ref) = do
  atomicModifyIORef' ref (\n -> (n + 1, ()))
  return $ VCons "(,)" [VAffineSender c ref, VAffineSender c ref]

-- cloneAR: duplicate the read cursor of a receiver end. Both results share
-- the same write end but have independent read cursors on the same stream.
cloneReceiver :: Value -> IO Value
cloneReceiver (VChan readEnd) = do
  readChan' <- C.dupChan (fst readEnd)
  let readEnd' = (readChan', snd readEnd)
  return $ VCons "(,)" [VChan readEnd, VChan readEnd']

-- drop: atomically decrement the shared count; the thread that observes it
-- hit zero writes the `Nothing` terminator. atomicModifyIORef' guarantees
-- exactly one caller sees 0, so exactly one Nothing is ever written.
dropSender :: Value -> IO Value
dropSender v =
  case v of
    VAffineSender c ref -> do
      n <- atomicModifyIORef' ref (\k -> (k - 1, k - 1))
      if n <= 0
        then send (VCons "Nothing" []) c $> VUnit
        else return VUnit
    _ -> error $ "dropSender: not a VAffineSender, got: " ++ show v


-- receiveA: receive one message; it is already Just/Nothing-shaped (written
-- by sendA / dropSender), so just pair the payload with the continuation.
receiveA :: Value -> IO Value
receiveA (VChan c) = do
  (v, c') <- receive c
  case v of
    VCons "Nothing" []  -> return $ VCons "NothingL" []
    VCons "Just"   [x]  -> return $ VCons "JustL" [VCons "(,)" [x, VChan c']]
    other               -> error ("receiveA: malformed affine message: " ++ show other)
receiveA other =
  error $ "receiveA: expected VChan, got: " ++ show other

builtins :: Map.Map String Value
builtins = Map.fromList
  [
  -- * Undefined
    ("undefined",     VBuiltin undefined)
  -- * Error

  -- * Standard types, classes and related functions
  -- ** Basic datatypes
  -- *** Logical operators
  , ("(&&)",          VBuiltin (\x -> VBuiltin (\y -> hsToFstBool (fstToHsBool x && fstToHsBool y))))
  , ("(||)",          VBuiltin (\x -> VBuiltin (\y -> hsToFstBool (fstToHsBool x || fstToHsBool y))))
  -- *** Strings
  , ("ord",           VBuiltin (\(VChar c) -> VInt (ord c)))
  , ("chr",           VBuiltin (\(VInt x) -> VChar (chr x)))
  , ("(^^)",          VBuiltin (\s1 -> VBuiltin (\s2 -> hsToFstString (fstToHsString s1 ++ fstToHsString s2))))
  , ("show",          VBuiltin (hsToFstString . unparse))
  -- ** Comparison
  , ("(<)",           VBuiltin (\(VInt x) -> VBuiltin (\(VInt y) -> hsToFstBool (x < y))))
  , ("(<=)",          VBuiltin (\(VInt x) -> VBuiltin (\(VInt y) -> hsToFstBool (x <= y))))
  , ("(==)",          VBuiltin (\(VInt x) -> VBuiltin (\(VInt y) -> hsToFstBool (x == y))))
  , ("(>=)",          VBuiltin (\(VInt x) -> VBuiltin (\(VInt y) -> hsToFstBool (x >= y))))
  , ("(>)",           VBuiltin (\(VInt x) -> VBuiltin (\(VInt y) -> hsToFstBool (x > y))))
  , ("(/=)",          VBuiltin (\(VInt x) -> VBuiltin (\(VInt y) -> hsToFstBool (x /= y))))
  , ("(>.)",          VBuiltin (\(VFloat x) -> VBuiltin (\(VFloat y) -> hsToFstBool (x > y))))
  , ("(<.)",          VBuiltin (\(VFloat x) -> VBuiltin (\(VFloat y) -> hsToFstBool (x < y))))
  , ("(>=.)",         VBuiltin (\(VFloat x) -> VBuiltin (\(VFloat y) -> hsToFstBool (x >= y))))
  , ("(<=.)",         VBuiltin (\(VFloat x) -> VBuiltin (\(VFloat y) -> hsToFstBool (x <= y))))
  -- ** Numeric functions
  -- *** Int
  , ("(+)",           VBuiltin (\(VInt x) -> VBuiltin (\(VInt y) -> VInt (x + y))))
  , ("(-)",           VBuiltin (\(VInt x) -> VBuiltin (\(VInt y) -> VInt (x - y))))
  , ("(*)",           VBuiltin (\(VInt x) -> VBuiltin (\(VInt y) -> VInt (x * y))))
  , ("(/)",           VBuiltin (\(VInt x) -> VBuiltin (\(VInt y) -> VInt (div x y))))
  , ("(^)",           VBuiltin (\(VInt x) -> VBuiltin (\(VInt y) -> VInt (x ^ y))))
  , ("subtract",      VBuiltin (\(VInt x) -> VBuiltin (\(VInt y) -> VInt (x - y))))
  , ("quot",          VBuiltin (\(VInt x) -> VBuiltin (\(VInt y) -> VInt (quot x y))))
  , ("rem",           VBuiltin (\(VInt x) -> VBuiltin (\(VInt y) -> VInt (rem x y))))
  , ("div",           VBuiltin (\(VInt x) -> VBuiltin (\(VInt y) -> VInt (div x y))))
  , ("mod",           VBuiltin (\(VInt x) -> VBuiltin (\(VInt y) -> VInt (mod x y))))
  , ("min",           VBuiltin (\(VInt x) -> VBuiltin (\(VInt y) -> VInt (min x y))))
  , ("max",           VBuiltin (\(VInt x) -> VBuiltin (\(VInt y) -> VInt (max x y))))
  , ("gcd",           VBuiltin (\(VInt x) -> VBuiltin (\(VInt y) -> VInt (gcd x y))))
  , ("lcm",           VBuiltin (\(VInt x) -> VBuiltin (\(VInt y) -> VInt (lcm x y))))
  , ("succ",          VBuiltin (\(VInt x) -> VInt (succ x)))
  , ("pred",          VBuiltin (\(VInt x) -> VInt (pred x)))
  , ("abs",           VBuiltin (\(VInt x) -> VInt (abs x)))
  , ("negate",        VBuiltin (\(VInt x) -> VInt (-x)))
  , ("even",          VBuiltin (\(VInt x) -> hsToFstBool (even x)))
  , ("odd",           VBuiltin (\(VInt x) -> hsToFstBool (odd x)))
  -- *** Float
  , ("(+.)",          VBuiltin (\(VFloat x) -> VBuiltin (\(VFloat y) -> VFloat (x + y))))
  , ("(-.)",          VBuiltin (\(VFloat x) -> VBuiltin (\(VFloat y) -> VFloat (x - y))))
  , ("(*.)",          VBuiltin (\(VFloat x) -> VBuiltin (\(VFloat y) -> VFloat (x * y))))
  , ("(/.)",          VBuiltin (\(VFloat x) -> VBuiltin (\(VFloat y) -> VFloat (x / y))))
  , ("(**)",          VBuiltin (\(VFloat x) -> VBuiltin (\(VFloat y) -> VFloat (x ** y))))
  , ("maxF",          VBuiltin (\(VFloat x) -> VBuiltin (\(VFloat y) -> VFloat (max x y))))
  , ("minF",          VBuiltin (\(VFloat x) -> VBuiltin (\(VFloat y) -> VFloat (min x y))))
  , ("logBase",       VBuiltin (\(VFloat x) -> VBuiltin (\(VFloat y) -> VFloat (logBase x y))))
  , ("absF",          VBuiltin (\(VFloat x) -> VFloat (abs x)))
  , ("negateF",       VBuiltin (\(VFloat x) -> VFloat (negate x)))
  , ("recip",         VBuiltin (\(VFloat x) -> VFloat (recip x)))
  , ("exp",           VBuiltin (\(VFloat x) -> VFloat (exp x)))
  , ("log",           VBuiltin (\(VFloat x) -> VFloat (log x)))
  , ("sqrt",          VBuiltin (\(VFloat x) -> VFloat (sqrt x)))
  , ("log1p",         VBuiltin (\(VFloat x) -> VFloat (log1p x)))
  , ("expm1",         VBuiltin (\(VFloat x) -> VFloat (expm1 x)))
  , ("log1pexp",      VBuiltin (\(VFloat x) -> VFloat (log1pexp x)))
  , ("log1mexp",      VBuiltin (\(VFloat x) -> VFloat (log1mexp x)))
  , ("sin",           VBuiltin (\(VFloat x) -> VFloat (sin x)))
  , ("cos",           VBuiltin (\(VFloat x) -> VFloat (cos x)))
  , ("tan",           VBuiltin (\(VFloat x) -> VFloat (tan x)))
  , ("asin",          VBuiltin (\(VFloat x) -> VFloat (asin x)))
  , ("acos",          VBuiltin (\(VFloat x) -> VFloat (acos x)))
  , ("atan",          VBuiltin (\(VFloat x) -> VFloat (atan x)))
  , ("sinh",          VBuiltin (\(VFloat x) -> VFloat (sinh x)))
  , ("cosh",          VBuiltin (\(VFloat x) -> VFloat (cosh x)))
  , ("tanh",          VBuiltin (\(VFloat x) -> VFloat (tanh x)))
  , ("truncate",      VBuiltin (\(VFloat x) -> VInt (truncate x)))
  , ("round",         VBuiltin (\(VFloat x) -> VInt (round x)))
  , ("ceiling",       VBuiltin (\(VFloat x) -> VInt (ceiling x)))
  , ("floor",         VBuiltin (\(VFloat x) -> VInt (floor x)))
  , ("pi",            VFloat pi)
  , ("fromInteger",   VBuiltin (\(VInt x) -> VFloat (fromInteger (toInteger x))))
  -- * Concurrency
  , ("fork",          VFork)
  , ("send",          VBuiltin (\val -> VBuiltin (\(VChan c) -> VIO $ VChan <$> send val c)))
  , ("receive",       VBuiltin (\(VChan c) -> VIO $ receive c >>= \(val, c) -> return $ VCons "(,)" [val, VChan c]))
  , ("wait",          VBuiltin wait)
  , ("close",         VBuiltin (VIO . close))
  , ("send_",         VBuiltin (\val -> VBuiltin (\(VChan c) -> VIO $ VUnit <$ send val c)))
  , ("receive_",      VBuiltin (\(VChan c) -> VIO $ receive c >>= \(val, c) -> return val))
  -- * affine channels
  -- '()' is parsed/evaluated as 'VCons "()" []' (empty-tuple constructor),
  -- NOT as 'VUnit'. The newA unit arg must therefore be matched with '_';
  -- otherwise GHC's runtime fires 'Non-exhaustive patterns in lambda' on
  -- every 'newA @T ()' call site.
  , ("newA",          VBuiltin (\_    -> VIO affineChan'))
  , ("sendA",         VBuiltin (\x -> VBuiltin (\c -> VIO $ sendA x c)))
  , ("cloneAS",       VBuiltin (\c -> VIO $ cloneSender c))
  , ("cloneAR",       VBuiltin (\c -> VIO $ cloneReceiver c) )
  , ("drop",          VBuiltin (\c -> VIO $ dropSender c))
  , ("receiveA",      VBuiltin (\c -> VIO $ receiveA c))
  -- waitA: receiver-side acknowledgement that the affine-receiver protocol
  -- has reached its 'Wait' terminator. At runtime this is a logical
  -- acknowledgement — the channel has already been drained by the time the
  -- 'NothingL' case fires (remember, 'drop' on the last living sender writes
  -- the 'Nothing' sentinel). We accept any value shape and return VUnit,
  -- mirroring the newA permissive pattern.
  , ("waitA",         VBuiltin (\_ -> VIO (return (VCons "()" []))))
  -- * I/O
  -- ** Standard I/O
  -- *** stdin
  -- **** Internal stdin functions
  , ("internalGetChar",       VBuiltin (const $ VIO $ VChar <$> getChar))
  , ("internalGetLine",       VBuiltin (const $ VIO $ hsToFstString <$> getLine))
  , ("internalGetContents",   VBuiltin (const $ VIO $ hsToFstString <$> getContents))
  , ("internalPutStrOut",     VBuiltin (\s -> VIO $ VUnit <$ putStr (fstToHsString s)))

  -- * Other Expressions
  , ("select",        VBuiltin (\(VLabel label) -> VBuiltin (\(VChan c) -> VIO $ VChan <$> sendLabel label c)))
  , ("sendType",      VBuiltin (\(VChan c) -> VIO $ VChan <$> send VUnit c))
  , ("receiveType",   VBuiltin (\(VChan c) -> VIO $ receive c >>= \(_, c) -> return $ VChan c))
  ]