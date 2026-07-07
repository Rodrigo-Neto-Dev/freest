module AffineKindingInvalidSpec (spec) where

import UnitSpecUtils
import Validation.Kinding ( runKindModule )

import Test.Hspec

-- | Drive the post-migration affine-channel invalid-kind tests.
--
-- Pattern follows 'KindInvalidSpec.hs': each `case T : K` line in
-- the .test file is an assertion that `T :: K` REJECTS under the
-- new capability/session architecture. 'errorsAreSuccesses' means
-- "if the type checker errored, the test passes".
--
-- Two .test files feed this spec:
--   * AffineWellFormedness.test — assertions with a kind annotation
--     that does NOT match the wrapper's actual kind (`1C`). Each
--     must be rejected.
--   * AffineChannelsIllFormedTypes.test — wrappers whose inner is
--     not a session-kind protocol. Each must be rejected (no kind
--     annotation; runSynthOrCheck synthesises the kind and that
--     synthesis must fail).
--
-- Under the post-migration architecture the wrapper is `1C` (linear
-- channel), so any annotation that is not `1C` (or `1S` via
-- subkinding) must be rejected at the kinding stage.
main :: IO ()
main = hspec spec

spec :: Spec
spec = mkTypeSpec
  [ "test/unit/AffineChannelsIllFormedTypes.test"
  ]
  "Invalid affine-channel kinding tests"
  errorsAreSuccesses
  \src -> \case
    (t, k, m) ->
      case runKindModule m >>= \(kctx, _) -> runSynthOrCheck kctx t k of
        Left _  -> return ()
        Right _ -> expectationFailure
                     ( "An error was expected but none was thrown. The .test line "
                     ++ "is no longer ill-formed under the post-migration architecture. "
                     ++ "(Type parsed as: " ++ show t ++ ")" )
