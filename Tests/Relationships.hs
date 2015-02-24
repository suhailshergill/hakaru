{-# LANGUAGE TypeFamilies, Rank2Types, FlexibleContexts #-}
module Tests.Relationships (allTests) where

import Prelude hiding (Real)

import Language.Hakaru.Syntax

import Test.HUnit
import Tests.TestTools

testRelationships :: Test
testRelationships = test [
    "t1"   ~: testSS [t1] (lam (\_ -> (lam (\_ -> normal 0 1)))),
    "t2"   ~: testSS [t2] (lam (\b -> gamma b 2)),
    "t3"   ~: testSS [t3, t3'] (lam (\_ -> (lam (\b -> gamma 2 b)))),
    "t7"   ~: testSS [t7] (normal 0 1 `bind` \x1 ->
                           normal 0 1 `bind` \x2 ->
                           dirac (x1 * recip x2)),
    "t8"   ~: testSS [t8] (lam (\a -> (lam (\alpha ->
                           (normal 0 1 `bind` \x1 ->
                           normal 0 1 `bind` \x2 ->
                           dirac (a + (fromProb alpha) * (x1 / x2)))))))
    ]

allTests :: Test
allTests = test [
    testRelationships
    ]

t1 :: (Lambda repr, Mochastic repr) => repr (Real -> Prob -> Measure Real)
t1 = lam (\mu -> (lam (\sigma -> normal mu sigma `bind` \x -> dirac ((x - mu) / (fromProb sigma)))))

t2 :: (Lambda repr, Mochastic repr) => repr (Prob -> Measure Prob)
t2 = lam (\b -> chi2 (2*b))

t3 :: (Lambda repr, Mochastic repr) => repr (Prob -> Prob -> Measure Prob)
t3 = lam (\alpha -> (lam (\bet -> gamma alpha bet `bind` \x -> dirac (2 * x / alpha))))

t3' :: (Lambda repr, Mochastic repr) => repr (Prob -> Prob -> Measure Prob)
t3' = (lam (\_ -> (lam (\bet -> chi2 (2*bet)))))

t4 :: (Lambda repr, Mochastic repr) => repr (Prob -> Prob -> Prob -> Measure Prob)
t4 = lam (\a -> lam (\b -> lam (\t -> 
  gamma a t `bind` \x1 -> 
  gamma b t `bind` \x2 -> 
  dirac (x1/(x1+x2)))))

t5 :: (Lambda repr, Mochastic repr) => repr (Prob -> Measure Prob)
t5 = lam (\alpha -> uniform 0 1 `bind` \x -> dirac (-alpha * unsafeProb(log_ (unsafeProb x))))

t5' :: (Lambda repr, Mochastic repr) => repr (Prob -> Measure Prob)
t5' = lam (\alpha -> laplace (fromProb alpha) alpha `bind` \x -> dirac (abs (unsafeProb x)))

t7 :: (Mochastic repr) => repr (Measure Real)
t7 = cauchy 0 1

t8 :: (Lambda repr, Mochastic repr) => repr (Real -> Prob -> Measure Real)
t8 = (lam (\a -> (lam (\alpha -> cauchy a alpha))))
