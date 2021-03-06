{-# LANGUAGE TypeFamilies, Rank2Types, FlexibleContexts, DeriveGeneric, 
  TemplateHaskell, UndecidableInstances, ConstraintKinds, DeriveDataTypeable
  , ScopedTypeVariables, DataKinds #-}
{-# OPTIONS -W -ddump-splices #-}

module Tests.Embed (allTests) where

import Language.Hakaru.Syntax (Hakaru(..),
       Order(..), Base(..), ununit, and_, fst_, snd_, swap_, min_,
       Mochastic(..), Lambda(..), Integrate(..), bind_, liftM, factor, beta, bern, lam)
import Language.Hakaru.Util.Pretty (Pretty (pretty), prettyPair)

import Control.Monad (zipWithM_, replicateM)
import Control.Applicative (Const(Const))
import Text.PrettyPrint (text, (<>), ($$), nest)
import Data.Function(on)
import Language.Hakaru.Sample
import Language.Hakaru.Embed
import Language.Hakaru.Maple 
import Language.Hakaru.Simplify 
import Control.Exception
import Data.Typeable 
import Test.HUnit
import Tests.TestTools
import Language.Hakaru.Any (Any(unAny))

import Tests.EmbedDatatypes 

-- Variant of testSS for Embeddable a 
type TesteeEmbed a =
  forall repr. (Mochastic repr, Integrate repr, Lambda repr, Embed repr) => repr a

testSE :: (Simplifiable a) => TesteeEmbed a -> Assertion
testSE t = do
    p <- simplify t `catch` handleSimplify t
    let s = result (unAny p)
    assertResult (show s)

testSSE :: (Simplifiable a) => [Maple a] -> TesteeEmbed a -> Assertion
testSSE ts t' =
    mapM_ (\t -> do p <- simplify t --`catch` handleSimplify t
                    (assertEqual "testSS" `on` result) t' (unAny p))
          (t' : ts)

-- YT: All of these tests should work now, but Maple seems to be broken as of
-- writing this because the names of all the built-in identifies have changed
-- (Measure to HMeasure) and Maple doesn't emit the new names yet, so they
-- haven't been run.

allTests :: Test
allTests = test 
  [ "pair-elim" ~: testSSE [t1] (uniform 1 2)
  , "P2-elim" ~: testSSE [t0] (uniform 1 2)  
  , "P2-id" ~: testSSE [t3] t3 
  , "List-id" ~: testSSE [testList0] testList0
  ]

testList0 :: Embed r => r (List HInt)
testList0 = fromL [1, 2, 3, 4]

t0 :: forall repr . (Mochastic repr, Embed repr) => repr (HMeasure HReal)
t0 = case_ (p2 1 2) (NFn (\x y -> uniform (unKonst x) (unKonst y)) :* Nil)

t1 :: forall repr . (Mochastic repr) => repr (HMeasure HReal)
t1 = unpair (pair 1 2) uniform 

t3 :: (Mochastic repr, Embed repr) => repr (HMeasure (P2 HInt HReal))
t3 = dirac (p2 1 2)

norm :: (Embed repr, Mochastic repr) => repr (HMeasure (P2 HReal HReal))
norm = normal 0 1 `bind` \x ->
       normal x 1 `bind` \y ->
       dirac (p2 x y)
