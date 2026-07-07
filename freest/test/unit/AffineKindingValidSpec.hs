module AffineKindingValidSpec (spec) where

import UI.Error ( showErrors )
import Validation.Kinding ( runKindModule, runCheck )
import UnitSpecUtils

import Test.Hspec

-- | Drive the post-migration affine-channel valid-kind tests.
--
-- Pattern follows 'KindCheckValidSpec.hs': each `case T : K` line in
-- the .test file is an assertion that `T :: K` SUCCEEDS under the new
-- capability/session architecture. 'mkTypeSpec' parses `.test` files
-- and feeds each parsed (type, kind, module) triple into the handler.
--
-- Under the post-migration architecture:
--   * Affine wrappers ('AffineSender' / 'AffineReceiver') are real
--     constructors whose inner is a session-kind (`1S`) protocol.
--   * The wrapper itself is kind `1C` (linear channel), enforced by
--     the 'Kinding' pass.
--   * Each entry below asserts that a wrapper carries a well-kinded
--     session protocol and the wrapper is also well-kinded at `1C`.
main :: IO ()
main = hspec spec

spec :: Spec
spec = mkTypeSpec
  [ "test/unit/AffineChannelKinds.test"
  ]
  "Valid affine-channel kinding tests"
  errorsAreFailures
  \src -> \case
    (t, Nothing, _) ->
      expectationFailure "Ill-formed affine test case: missing kind annotation"
    (t, Just k, m) ->
      case runKindModule m >>= \(kctx, _) -> runCheck kctx t k of
        Left es -> expectationFailure
                     ( showErrors src es
                     ++ "\n(type parsed as `" ++ show t ++ "`)"
                     )
        Right _ -> return ()
