{-# LANGUAGE TypeFamilies, Rank2Types, FlexibleContexts, DataKinds #-}
{-# OPTIONS -W #-}

module Tests.Syntax(allTests) where

import Prelude hiding (Real)
import Language.Hakaru.Syntax (Hakaru(..),
       Order(..), Base(..), ununit, and_, fst_, snd_, swap_, min_, max_,
       Mochastic(..), Lambda(..), Integrate(..), bind_, liftM, liftM2, factor, beta, bern, exponential)
import Language.Hakaru.Util.Pretty (Pretty (pretty), prettyPair)
import qualified Language.Hakaru.Lazy as L
import Language.Hakaru.Any (Any(Any))

import Control.Monad (zipWithM_)
import Control.Applicative (Const(Const))
import Text.PrettyPrint (text, (<>), ($$), nest)

import Test.HUnit
import Tests.TestTools


allTests :: Test
allTests = test [
    "pair1fst/snd" ~: testSS [pair1fst, pair1snd] pair1same,
    "pair2fst" ~: testS pair2fst,
    "pair2snd" ~: testS pair2snd,
    -- "pair2'fst" ~: testS $ pair2'fst 10,
    -- "pair2'snd" ~: testS $ pair2'snd 10,
    -- "replicateH" ~: testS $ replicateH 10,
    "pair3fst" ~: testS $ pair3fst 1 [true,true,true],
    "pair3snd" ~: testS $ pair3snd 1 [true,true,true],
    "pair3trd" ~: testS $ pair3trd 1 [true,true,true],
    "pair4fst" ~: testS $ pair4fst,
    "pair4transition" ~: testS $ pair4transition $ pair true 1,
    "pair4'transition" ~: testS $ pair4'transition $ pair true 1,
    -- "transitionTest" ~: ignore $ transitionTest,
    --"testDistWithSample" ~: ignore $ do x <- testDistWithSample
    --                                    mapM_ assertJust x,
    "testLinregSimp" ~: testSS [linregSimp] linregSimp',
    "testdistLinregSimp" ~: testS distLinregSimp,
    "testLinreg" ~: ignore $ testS distLinreg,
    "gamalonDis" ~: testS gamalonDis ]

-- pair1fst and pair1snd are equivalent
pair1fst :: (Mochastic repr) => repr (HMeasure (HPair HBool HProb))
pair1fst =  beta 1 1 `bind` \bias ->
            bern bias `bind` \coin ->
            dirac (pair coin bias)
pair1snd :: (Mochastic repr) => repr (HMeasure (HPair HBool HProb))
pair1snd =  bern (1/2) `bind` \coin ->
            if_ coin (beta 2 1) (beta 1 2) `bind` \bias ->
            dirac (pair coin bias)

pair1same :: (Mochastic repr) => repr (HMeasure (HPair HBool HProb))
pair1same = superpose [
  ( fromRational (1/2), beta 2 1 `bind` \x -> dirac (pair true x) ) ,
  ( fromRational (1/2), beta 1 2 `bind` \x -> dirac (pair false x) ) ]
  

-- pair2fst and pair2snd are equivalent
pair2fst
    :: (Mochastic repr)
    => repr (HMeasure (HPair (HPair HBool HBool) HProb))
pair2fst =  beta 1 1 `bind` \bias ->
            bern bias `bind` \coin1 ->
            bern bias `bind` \coin2 ->
            dirac (pair (pair coin1 coin2) bias)

pair2snd
    :: (Mochastic repr)
    => repr (HMeasure (HPair (HPair HBool HBool) HProb))
pair2snd =  bern (1/2) `bind` \coin1 ->
            bern (if_ coin1 (2/3) (1/3)) `bind` \coin2 ->
            beta (1 + f coin1 + f coin2)
                 (1 + g coin1 + g coin2) `bind` \bias ->
            dirac (pair (pair coin1 coin2) bias)
  where f b = if_ b 1 0
        g b = if_ b 0 1

{-
type Cont repr a = forall w. (a -> repr (HMeasure w)) -> repr (HMeasure w)
-- This Cont monad is useful for generalizing pair2fst and pair2snd to an
-- arbitrary number of coin flips. The generalization would look liks this:

pair2'fst :: (Mochastic repr) => Int -> Cont repr ([repr Bool], repr Prob)
-- REQUIREMENT: pair2fst = pair2'fst 2 (\([coin1,coin2],bias) -> dirac (pair (pair coin1 coin2) bias))
pair2'fst n k = beta 1 1 `bind` \bias ->
                replicateH n (bern bias) (\ coins -> k (coins, bias))

pair2'snd :: (Mochastic repr) => Int -> Cont repr ([repr Bool], repr Prob)
pair2'snd = go 1 1 where
  go a b 0 k = beta a b `bind` \bias -> k ([],bias)
  go a b n k = bern (a/(a+b)) `bind` \c ->
               go (if_ c (a+1) a) (if_ c b (b+1)) (n-1) (\(cs,bias) ->
               k (c:cs,bias))

replicateH :: (Mochastic repr) => Int -> repr (HMeasure a) -> Cont repr [repr a]
replicateH 0 _ k = k []
replicateH n m k = m `bind` \x -> replicateH (n-1) m (\xs -> k (x:xs))

twice :: (Mochastic repr) => repr (HMeasure a) -> Cont repr (repr a, repr a)
twice m k = m `bind` \x ->
            m `bind` \y ->
            k (x, y)
-}

-- pair3fst and pair3snd and pair3trd are equivalent
pair3fst, pair3snd, pair3trd
    :: (Mochastic repr)
    => repr HProb
    -> [repr HBool]
    -> repr (HMeasure HUnit)
pair3fst bias [b1,b2,b3] =
  factor (if_ b1 bias (1-bias)) `bind_`
  factor (if_ b2 bias (1-bias)) `bind_`
  factor (if_ b3 bias (1-bias))
pair3fst _ _ = error "pair3fst: only implemented for 3 coin flips"
pair3snd bias [b1,b2,b3] =
  factor (if_ b1 bias (1-bias)
        * if_ b2 bias (1-bias)
        * if_ b3 bias (1-bias))
pair3snd _ _ = error "pair3fst: only implemented for 3 coin flips"
pair3trd bias [b1,b2,b3] =
  factor (pow_ bias     (if_ b1 1 0 + if_ b2 1 0 + if_ b3 1 0)
        * pow_ (1-bias) (if_ b1 0 1 + if_ b2 0 1 + if_ b3 0 1))
pair3trd _ _ = error "pair3fst: only implemented for 3 coin flips"

pair4fst :: (Mochastic repr) => repr (HMeasure HReal)
pair4fst = bern (1/2) `bind` \coin ->
           if_ coin (normal 0 1) (uniform 0 1)

normalLogDensity :: Mochastic repr => repr HReal -> repr HProb -> repr HReal -> repr HReal
normalLogDensity mu sd x = (-(fromProb tau) * square (x - mu)
                            + log_ (tau / pi_ / 2)) / 2
 where square y = y * y
       tau = recip (square sd)

uniformLogDensity :: Mochastic repr => repr HReal -> repr HReal -> repr HReal -> repr HReal
uniformLogDensity lo hi x = if_ (and_ [less lo x, less x hi])
                            (log_ (recip (unsafeProb (hi - lo))))
                            (log_ 0)

bernLogDensity :: Mochastic repr => repr HProb -> repr HBool -> repr HReal
bernLogDensity p x = log_ (if_ x p (1 - p))

pair4transition
    :: Mochastic repr
    => repr (HPair HBool HReal)
    -> repr (HMeasure (HPair HBool HReal))
pair4transition state = bern (1/2) `bind` \resampleCoin ->
                           if_ resampleCoin
                           (bern (1/2) `bind` \coin' ->
                            densityCheck (coin',x))
                           (if_ coin
                            (normal 3 2 `bind` \y -> densityCheck (coin, y))
                            (uniform (-1) 1 `bind` \y -> densityCheck (coin, y)))
    where densityCheck (coin', x') = if_ (less (bernLogDensity (1/2) coin' +
                                                 (if_ coin'
                                                  (normalLogDensity 0 1 x')
                                                  (uniformLogDensity 0 1 x')) -
                                                 bernLogDensity (1/2) coin -
                                                 (if_ coin
                                                  (normalLogDensity 0 1 x)
                                                  (uniformLogDensity 0 1 x))) 0)
                                       (dirac state)
                                       (dirac (pair coin' x'))
          coin = fst_ state
          x = snd_ state

pair4'transition
    :: (Mochastic repr)
    => repr (HPair HBool HReal)
    -> repr (HMeasure (HPair HBool HReal))
pair4'transition state = bern (1/2) `bind` \resampleCoin ->
                           if_ resampleCoin
                           (bern (1/2) `bind` \coin' ->
                            (if_ coin'
                             (updateState (pair coin' x) $
                              uniformLogDensity 0 1 x - normalLogDensity 0 1 x)
                             (updateState (pair coin' x) $
                              normalLogDensity 0 1 x - uniformLogDensity 0 1 x)))
                           (if_ coin
                            (normal 3 2 `bind` \x' ->
                                 updateState (pair coin x')
                                   (normalLogDensity 0 1 x' - normalLogDensity 0 1 x))
                            (uniform (-1) 1 `bind` \x' ->
                                 updateState (pair coin x')
                                   (uniformLogDensity 0 1 x' - uniformLogDensity 0 1 x)))
    where updateState state' ratio = bern (min_ 1 (exp_ ratio)) `bind` \u ->
                                       if_ u (dirac state') (dirac state)
          coin = fst_ state
          x = snd_ state

{-
transitionTest :: MWC.GenIO -> IO (Maybe ((Bool, Double), LF.LogFloat))
transitionTest g = unSample (pair4transition (pair true 1)) 1 g

testDistWithSample :: IO [Maybe (Double, LF.LogFloat)]
testDistWithSample = do
  g <- MWC.create
  let prog2 = (head prog 0) 1
  replicateM 10 (unSample prog2 1 g)
 where prog = runDisintegrate $ \env ->
                                normal env 1 `bind` \x ->
                                normal x 1 `bind` \y ->
                                dirac (pair y x)
-}

type Real5 = (HPair HReal (HPair HReal (HPair HReal (HPair HReal HReal))))
type Real6 = (HPair HReal Real5)

make5Pair :: (Base repr) => repr a -> repr a -> repr a -> repr a -> repr a -> repr (HPair a (HPair a (HPair a (HPair a a))))
make5Pair x1 x2 x3 x4 x5 =
    pair x1 (pair x2 (pair x3 (pair x4 x5)))

make6Pair :: (Base repr) => repr a -> repr a -> repr a -> repr a -> repr a -> repr a -> repr (HPair a (HPair a (HPair a (HPair a (HPair a a)))))
make6Pair x1 x2 x3 x4 x5 x6 =
    pair x1 (pair x2 (pair x3 (pair x4 (pair x5 x6))))

linregSimp, linregSimp'
    :: Mochastic repr
    => repr (HMeasure (HPair (HPair HReal HReal) HReal))
linregSimp = 
         normal 0 2 `bind` \w ->
         uniform (-1) 1 `bind` \x ->
         uniform (-1) 1 `bind` \_ ->
         uniform (-1) 1 `bind` \_ ->
         uniform (-1) 1 `bind` \_ ->
         uniform (-1) 1 `bind` \_ ->
         normal (x*w) 1 `bind` \y ->
         dirac (pair (pair x y) w)
linregSimp' =
         normal 0 2 `bind` \w ->
         uniform (-1) 1 `bind` \x ->
         normal (x*w) 1 `bind` \y ->
         dirac (pair (pair x y) w)

distLinregSimp
    :: (Lambda repr, Mochastic repr)
    => repr (HFun (HPair HReal HReal) (HMeasure HReal))
distLinregSimp = app (L.runDisintegrate (const linregSimp) !! 0) unit

linreg :: Mochastic repr => repr (HMeasure (HPair Real6 Real5))
linreg = normal 0 2 `bind` \w1 ->
         normal 0 2 `bind` \w2 ->
         normal 0 2 `bind` \w3 ->
         normal 0 2 `bind` \w4 ->
         normal 0 2 `bind` \w5 ->
         uniform (-1) 1 `bind` \x1 ->
         uniform (-1) 1 `bind` \x2 ->
         uniform (-1) 1 `bind` \x3 ->
         uniform (-1) 1 `bind` \x4 ->
         uniform (-1) 1 `bind` \x5 ->
         normal (x1*w1 + x2*w2 + x3*w3 + x4*w4 + x5*w5) 1 `bind` \y ->
         dirac (pair (make6Pair x1 x2 x3 x4 x5 y) (make5Pair w1 w2 w3 w4 w5))

distLinreg
    :: (Lambda repr, Mochastic repr)
    => repr (HFun Real6 (HMeasure Real5))
distLinreg = app (L.runDisintegrate (const linreg) !! 0) unit

{-
disintegrateTestRunner :: IO ()
disintegrateTestRunner = do
  testDist ( Bind (Leaf x) stdRandom
           $ Bind (Leaf y) stdRandom
           $ Dirac (Pair (exp' (Var x)) (Op2 Add (Var y) (Var x)))
           , Fst Root )
  testDist ( Bind (Leaf x) stdRandom
           $ Bind (Leaf y) stdRandom
           $ Dirac (Pair (exp' (Var x)) (Op2 Add (Var y) (Var x)))
           , Snd Root )
  testDist ( Bind (Leaf x) stdRandom
           $ Bind (Leaf y) stdRandom
           $ Bind (Leaf z) (D.max_ (Var x) (Var y))
           $ Dirac (Pair (Var z) (Pair (Var x) (Var y)))
           , Fst Root )
  testDist ( Bind (Leaf x) stdRandom
           $ Dirac (Pair (exp' (Var x)) (Op1 Neg (Var x)))
           , Fst Root )
  testDist ( Bind (Leaf x) (Choice [stdRandom, stdRandom])
           $ Bind (Leaf y) (Choice [stdRandom, stdRandom])
           $ Bind (Leaf z) (Choice [stdRandom, stdRandom])
           $ Dirac (Var x + Var y + Var z)
           , Root)
  testDist ( Bind (Leaf x) stdRandom
           $ Bind (Leaf y) stdRandom
           $ Dirac (Pair (Var x + Var y) (Var x - Var y))
           , Fst Root )
  testDist ( Bind (Leaf y) stdRandom
           $ Bind (Leaf x) stdRandom
           $ Dirac (Pair (Var x + Var y) (Bind (Leaf x) stdRandom (Dirac (Var y))))
           , Fst Root )
  testDist ( Bind (Leaf m) (Dirac (Bind (Leaf x) stdRandom (Dirac (Op2 Add 1 (Var x)))))
           $ Bind (Leaf x) (Var m)
           $ Bind (Leaf y) (Var m)
           $ Dirac (Pair (Var x) (Var y))
           , Fst Root )
  testDist ( Bind (Leaf m) (Bind (Leaf x) stdRandom (Dirac (Dirac (Op2 Add 1 (Var x)))))
           $ Bind (Leaf x) (Var m)
           $ Bind (Leaf y) (Var m)
           $ Dirac (Pair (Var x) (Var y))
           , Fst Root )
  where x, y, z :: Name Real
        x = Const "x"
        y = Const "y"
        z = Const "z"
        m :: Name (Measure Real)
        m = Const "m"
        exp' :: Expr Name Name Real -> Expr Name Name Real
        exp' = Op1 Exp

testDist :: (Expr Name Name (Measure t), Selector to t) -> IO ()
testDist (e,s) = do
  print (prettyPair (pretty' 0 e) (pretty' 0 s))
  let es = run (disintegrate e emptyEnv s (Var (Const 0)))
  putStrLn (show (length es) ++ " result(s):")
  zipWithM_ (\n d -> print (pretty n <> text "." $$ nest 3 (pretty' 0 d)))
            [1::Int ..]
            es
  putStrLn ""
-}

-- Simplify me! 2015-01-20 meeting                   
gamalonDis
    :: (Lambda repr, Mochastic repr, Integrate repr)
    => repr
        (HFun
            (HPair
                (HPair (HPair HReal HReal) (HPair HReal HReal))
                (HPair HReal HReal))
        (HFun (HPair HReal HReal)
            (HMeasure HProb)))
gamalonDis = lam $ \abcd_xy -> lam $ \x'y' ->
             dirac ((head (L.density gamalon)) abcd_xy x'y')

gamalon
    :: (Mochastic repr)
    => repr
        (HPair (HPair (HPair HReal HReal) (HPair HReal HReal))
            (HPair HReal HReal))
    -> repr (HMeasure (HPair HReal HReal))
gamalon abcd_xy =
    unpair abcd_xy $ \abcd xy ->
    unpair abcd $ \ab cd ->
    unpair ab $ \a b ->
    unpair cd $ \c d ->
    unpair xy $ \x y ->
    uniform (a * x + b * y - 1) (a * x + b * y + 1) `bind` \x' ->
    uniform (c * x + d * y - 1) (c * x + d * y + 1) `bind` \y' ->
    dirac (pair x' y')

walk
    :: (Mochastic repr)
    => repr HProb
    -> repr HReal
    -> repr (HMeasure (HPair HReal HInt))
walk remaining location
  = exponential 1 `bind` \elapsed ->
    if_ (less elapsed remaining)
        (normal location 1 `bind` \location' ->
         walk (remaining - elapsed) location' `bind` \p ->
         unpair p $ \location'' steps ->
         dirac (pair location'' (steps + 1)))
        (dirac (pair location 0))
