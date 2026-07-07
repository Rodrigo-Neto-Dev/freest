module AffineEquivalenceSpec (spec) where

import Syntax.Module qualified as M
import Validation.TypeEquivalence ( equivalent )
import UnitSpecUtils ( mkEquivalenceSpec )

import Test.Hspec

-- | Drive the post-migration affine-channel type-equivalence tests.
-- Pattern follows 'BisimulationValidSpec.hs': each entry in the .test
-- file is an assertion that two types are equivalent via structural
-- Eq or W-grammar bisimulation. 'mkEquivalenceSpec' parses `.test`
-- files and feeds each parsed (t, u, kind, modl) quadruple to the
-- handler. For reflexive entries (a wrapper with itself), the
-- congruence-based Eq on 'Type' recognises equality directly.
--
-- NOTE: the W-grammar treats 'AffineSender' / 'AffineReceiver' as
-- opaque constructors whose inner is reduced via standard bisimulation.
-- The wrapper itself does NOT collapse with its dual in the grammar
-- (e.g. 'Dual **?(s)' and '**!(Dual s)' are NOT bisimilar). So only
-- reflexivity entries are present.
main :: IO ()
main = hspec spec

spec :: Spec
spec = mkEquivalenceSpec
  [ "test/unit/AffineChannelTypeEquivalence.test"
  ]
  "Affine-channel type equivalence tests"
  \src (t, u, _k, m) -> equivalent (M.typeDecls m) t u
                          `shouldBe` True
