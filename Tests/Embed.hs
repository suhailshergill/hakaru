{-# LANGUAGE TypeFamilies, Rank2Types, FlexibleContexts, DeriveGeneric, 
  TemplateHaskell, UndecidableInstances, ConstraintKinds, 
  DeriveDataTypeable, ScopedTypeVariables, DataKinds #-}
{-# OPTIONS -W -ddump-splices #-}

module Tests.Embed (allTests) where

import Prelude hiding (Real)
import Language.Hakaru.Syntax (Real, Prob, Measure,
       Order(..), Base(..), ununit, and_, fst_, snd_, swap_, min_,
       Mochastic(..), Lambda(..), Integrate(..), bind_, liftM, factor, beta, bern, lam)
import Language.Hakaru.Util.Pretty (Pretty (pretty), prettyPair)
-- import Language.Hakaru.Sample(Sample(unSample))
import Language.Hakaru.Disintegrate hiding (Nil)

import Control.Monad (zipWithM_, replicateM)
import Control.Applicative (Const(Const))
import Text.PrettyPrint (text, (<>), ($$), nest)
import Generics.SOP hiding (Code) 
import GHC.Generics as GHC 
import Data.Function(on)
import Language.Hakaru.Sample
import Language.Hakaru.Expect
import Language.Hakaru.Embed
import Language.Hakaru.Maple 
import Language.Hakaru.Simplify 
import Control.Exception
import Data.Typeable 
import Test.HUnit
import Tests.TestTools
import Language.Hakaru.Any (Any(unAny))

import qualified Generics.SOP as SOP 
import qualified GHC.Generics as GHC  

import Generics.SOP (HasDatatypeInfo, Generic) 
import GHC.Generics (Generic)


-- Variant of testSS for Embeddable a 
type TesteeEmbed a =
  forall repr. (Mochastic repr, Integrate repr, Lambda repr, Embed repr) => repr a

testSE :: (Simplifiable a) => TesteeEmbed a -> Assertion
testSE t = do
    p <- simplify t `catch` handleSimplify t
    let s = result (unAny p)
    assertResult (show s)

testSSE :: (Simplifiable a) => [Expect Maple a] -> TesteeEmbed a -> Assertion
testSSE ts t' =
    mapM_ (\t -> do p <- simplify t --`catch` handleSimplify t
                    (assertEqual "testSS" `on` result) t' (unAny p))
          (t' : ts)

-- Instances for "built in" types. 
-- These shouldn't be used while "pair", "uneither" still exist.
instance Embeddable (a,b) where 
  type Code (a,b) = '[ '[a,b]]

instance Embeddable (Either a b) where 
  type Code (Either a b) = '[ '[a], '[b] ] 

-- Recursive occurences of some type T should appear as `HRep T` in Code 
instance Embeddable [a] where 
  type Code [a] = '[ '[] , '[a, HRep [a]] ] 

cons :: Embed repr => repr a -> repr (HRep [a]) -> repr (HRep [a])
cons x xs = sop (S $ Z $ x :* xs :* Nil)

nil :: Embed repr => repr (HRep [a]) 
nil = sop (Z Nil)


data Real5 = Real5 Real Real Real Real Real deriving GHC.Generic
$(deriveEmbeddable ''Real5)

data BoolProb = BoolProb Bool Prob deriving (GHC.Generic, Typeable)
$(deriveEmbeddable ''BoolProb)

data P2 a b = P2 a b deriving (GHC.Generic, Typeable)
$(deriveEmbeddable ''P2)


fstP2 :: Embed repr => repr (HRep (P2 a b)) -> repr a 
fstP2 x = case_ x (NFn (\a _ -> a) :* Nil)

sndP2 :: Embed repr => repr (HRep (P2 a b)) -> repr b 
sndP2 x = case_ x (NFn (\_ a -> a) :* Nil)

p2 :: Embed repr => repr a -> repr b -> repr (HRep (P2 a b))
p2 a b = sop (Z $ a :* b :* Nil)




-- Test must come after Template Haskell splices

allTests :: Test
allTests = test 
  [
    -- "P2-elim" ~: testSSE [t0] (uniform 1 2) 
  , "pair-elim" ~: testSSE [t1] (uniform 1 2) 
  ]

-- t0 :: forall repr . (Mochastic repr, Embed repr) => repr (Measure Real)
-- t0 = case_ (p2 1 2) (NFn uniform :* Nil)

t1 :: forall repr . (Mochastic repr) => repr (Measure Real)
t1 = unpair (pair 1 2) uniform 



-- data P2 a b = P2 a b deriving GHC.Generic
-- $(deriveEmbeddable ''P2)

-- deriveEmbeddable' [d| data P2 a b = P2 a b |]

-- test0 :: Expect Maple (HRep (P2 (Measure Prob) (Measure Prob)))
-- test0 = sop (Z $ dirac 1 :* dirac 2 :* Nil)

-- test1 :: Expect Maple (Measure Prob, Measure Prob)
-- test1 = pair (dirac 1) (dirac 2) 

-- -- test1 :: Expect Maple (Measure Prob, Measure Prob)
-- -- test1 = pair (dirac 1) (dirac 2) 

-- test2 :: Expect Maple (Measure (Prob, Prob))
-- test2 = case_ test0 (NFn (\x y -> bind x (\x' -> bind y (\y' -> dirac $ pair x' y'))) :* Nil)

-- runEMaple = \x -> runMaple (unExpect x) 0



                     


