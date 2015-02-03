{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
    TypeFamilies, StandaloneDeriving, GeneralizedNewtypeDeriving, GADTs,
    RankNTypes, ScopedTypeVariables, UndecidableInstances, TypeOperators, DataKinds, DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS -Wall #-}

-- module Language.Hakaru.Expect (Expect(..), Expect', total, normalize) where
module Language.Hakaru.Expect where

-- Expectation interpretation

import Prelude hiding (Real)
import Language.Hakaru.Syntax (Real, Prob, Measure,
       Order(..), Base(..), Mochastic(..), Integrate(..), Lambda(..))
import Language.Hakaru.Embed
import Language.Hakaru.Maple 
import qualified Generics.SOP as SOP
import Generics.SOP (I (..), (:.:) (..))

newtype Expect repr a = Expect { unExpect :: repr (Expect' a) }
type family Expect' (a :: *)
-- This type family must be changed in lockstep with typeExpect below
type instance Expect' Int          = Int
type instance Expect' Real         = Real
type instance Expect' Prob         = Prob
type instance Expect' Bool         = Bool
type instance Expect' ()           = ()
type instance Expect' (a, b)       = (Expect' a, Expect' b)
type instance Expect' (Either a b) = Either (Expect' a) (Expect' b)
type instance Expect' [a]          = [Expect' a]
type instance Expect' (Measure a)  = (Expect' a -> Prob) -> Prob
type instance Expect' (a -> b)     = (Expect' a -> Expect' b)

instance (Order repr Real) => Order (Expect repr) Real where
  less  (Expect x) (Expect y) = Expect (less  x y)
  equal (Expect x) (Expect y) = Expect (equal x y)

deriving instance (Eq         (repr Real)) => Eq         (Expect repr Real)
deriving instance (Ord        (repr Real)) => Ord        (Expect repr Real)
deriving instance (Num        (repr Real)) => Num        (Expect repr Real)
deriving instance (Fractional (repr Real)) => Fractional (Expect repr Real)
deriving instance (Floating   (repr Real)) => Floating   (Expect repr Real)

instance (Order repr Prob) => Order (Expect repr) Prob where
  less  (Expect x) (Expect y) = Expect (less  x y)
  equal (Expect x) (Expect y) = Expect (equal x y)

deriving instance (Eq         (repr Prob)) => Eq         (Expect repr Prob)
deriving instance (Ord        (repr Prob)) => Ord        (Expect repr Prob)
deriving instance (Num        (repr Prob)) => Num        (Expect repr Prob)
deriving instance (Fractional (repr Prob)) => Fractional (Expect repr Prob)

instance (Order repr Int) => Order (Expect repr) Int where
  less  (Expect x) (Expect y) = Expect (less  x y)
  equal (Expect x) (Expect y) = Expect (equal x y)

deriving instance (Eq  (repr Int)) => Eq  (Expect repr Int)
deriving instance (Ord (repr Int)) => Ord (Expect repr Int)
deriving instance (Num (repr Int)) => Num (Expect repr Int)

instance (Base repr) => Base (Expect repr) where
  unit                           = Expect unit
  pair (Expect a) (Expect b)     = Expect (pair a b)
  unpair (Expect ab) k           = Expect (unpair ab (\a b ->
                                   unExpect (k (Expect a) (Expect b))))
  inl (Expect a)                 = Expect $ inl a
  inr (Expect b)                 = Expect $ inr b
  uneither (Expect ab) ka kb     = Expect $ uneither ab (unExpect . ka . Expect)
                                                        (unExpect . kb . Expect)
  true                           = Expect true
  false                          = Expect false
  if_ eb (Expect et) (Expect ef) = Expect $ if_ (unExpect eb) et ef

  nil                            = Expect nil
  cons (Expect a) (Expect as)    = Expect $ cons a as
  unlist (Expect as) kn kc       = Expect $ unlist as (unExpect kn) (\a' as' ->
                                   unExpect (kc (Expect a') (Expect as')))
                                   
  unsafeProb                     = Expect . unsafeProb . unExpect
  fromProb                       = Expect . fromProb   . unExpect
  fromInt                        = Expect . fromInt    . unExpect
  pi_                            = Expect pi_
  exp_                           = Expect . exp_  . unExpect
  log_                           = Expect . log_  . unExpect
  sqrt_                          = Expect . sqrt_ . unExpect
  erf                            = Expect . erf   . unExpect
  erf_                           = Expect . erf_  . unExpect
  pow_ (Expect x) (Expect y)     = Expect (pow_ x y)
  infinity                       = Expect infinity
  negativeInfinity               = Expect negativeInfinity
  gammaFunc (Expect n)           = Expect (gammaFunc n)
  betaFunc (Expect a) (Expect b) = Expect (betaFunc a b)
  fix f                          = Expect (fix (unExpect . f . Expect))

instance (Integrate repr) => Integrate (Expect repr) where
  integrate (Expect lo) (Expect hi) f =
    Expect (integrate lo hi (unExpect . f . Expect))
  summate (Expect lo) (Expect hi) f =
    Expect (summate lo hi (unExpect . f . Expect))

instance (Integrate repr, Lambda repr)
      => Mochastic (Expect repr) where
  dirac (Expect a)  = Expect (lam (\c -> c `app` a))
  bind (Expect m) k = Expect (lam (\c -> m `app` lam (\a ->
                      unExpect (k (Expect a)) `app` c)))
  lebesgue          = Expect (lam (integrate negativeInfinity infinity . app))
  counting          = Expect (lam (summate   negativeInfinity infinity . app))
  superpose pms     = Expect (lam (\c -> sum [ p * app m c
                                             | (Expect p, Expect m) <- pms ]))
  uniform (Expect lo) (Expect hi) = Expect (lam (\f ->
    integrate lo hi (\x -> app f x / unsafeProb (hi - lo))))
  -- TODO: override poisson, gamma, invgamma to express that they do not
  --       generate negative numbers

instance (Lambda repr) => Lambda (Expect repr) where
  lam f = Expect (lam (unExpect . f . Expect))
  app (Expect rator) (Expect rand) = Expect (app rator rand)

total :: (Lambda repr, Base repr) => Expect repr (Measure a) -> repr Prob
total m = unExpect m `app` lam (\_ -> 1)

normalize :: (Integrate repr, Lambda repr, Mochastic repr) =>
             (forall repr'. (Integrate repr', Lambda repr', Mochastic repr') =>
                            (forall b. (Expect' b ~ b) => repr b -> repr' b) ->
                            repr' (Measure a)) ->
             repr (Measure a)
normalize m = superpose [(recip (total (m Expect)), m id)]



-- This doesn't work, and maybe it shouldn't 

{-
sopExpect :: forall r t . (SingI (Code t), Embed r, Embeddable t) 
          => NS (NP (Expect r)) (Code t) -> Expect r (HRep t) 
sopExpect x = Expect $ sop $ SOP.unSOP $ 
                SOP.hliftA (unsafeUnwrap . unExpect) (SOP.SOP x) where 

  -- unsafeUnwrap :: forall a . exists b . r (Expect' a) -> r b 
  unsafeUnwrap :: forall a . r (Expect' a) -> r a 
  unsafeUnwrap = undefined 


caseExpect :: Expect r (HRep t) -> NP (NFn (Expect r) o) (Code t) -> Expect r o 
caseExpect (Expect x) = _
-}


-- test0 :: Expect Maple (HRep (P2 (Measure Prob) (Measure Prob)))
--       == Maple (HRep (P2 (Measure Prob) (Measure Prob)))
--       == Maple (HRep ( P2 ((Prob -> Prob) -> Prob) ((Prob -> Prob) -> Prob) )) <- what we want 
-- test0 = sop (Z $ dirac 1 :* dirac 2 :* Nil)

-- data X = X (Measure Prob) (Measure Prob) 

-- test0' = sop (Z $ dirac 1 :* dirac 2 :* Nil)
-- test0' :: Expect Maple (HRep X)
--        == Maple (HRep X) 


-- test1 :: Expect Maple (Measure Prob, Measure Prob)
--       == Maple ((Prob -> Prob) -> Prob, (Prob -> Prob) -> Prob)
-- test1 = pair (dirac 1) (dirac 2) 

data Void 
type instance Expect' Void = Void 
type instance Expect' (HRep t) = HRep t  


data HSing t where 
  HProb :: HSing Prob
  HMeasure :: HSing a -> HSing (Measure a)
  HArr :: HSing a -> HSing b -> HSing (a -> b)

data a :~: b where 
  Refl :: a :~: a 

eqHSing :: HSing t0 -> HSing t1 -> Maybe (t0 :~: t1) 
eqHSing = undefined

class HakaruType (t :: *) where 
  hsing :: HSing t 

instance HakaruType Prob where 
  hsing = HProb 

instance HakaruType a => HakaruType (Measure a) where 
  hsing = HMeasure hsing 

instance (HakaruType a, HakaruType b) => HakaruType (a -> b) where 
  hsing = HArr hsing hsing 

-- type instance Expect' (Measure a) = (Expect' a -> Prob) -> Prob

-- sopExpect :: (SingI xss, Embed r) => (forall yss . NS (NP r) yss -> r Void) -> NS (NP (Expect r)) xss -> Expect r Void 
-- sopExpect sopR x = Expect $ sopR $ SOP.unSOP $ SOP.hliftA _ (SOP.SOP x)

class Embed' r where 
  fromVoid :: Embeddable t => r Void -> r (HRep t)
  toVoid :: Embeddable t => r (HRep t) -> r Void 

  sop'' :: (Int, [HTypeE r]) -> r Void 
  unsop'' :: r Void -> (Int, [HTypeE r])

  case'' :: r Void -> [[HTypeE r] -> r o] -> r o
  case'' x fs = case unsop'' x of 
                  (c, v) -> (fs !! c) v 

-- expect' :: (HakaruType (Expect' t), HakaruType t) => Expect r t -> r (Expect' t) 
-- expect' = undefined

hsingDict :: HSing a -> Dict (HakaruType a)
hsingDict HProb = Dict 
hsingDict (HMeasure a) = case hsingDict a of Dict -> Dict
hsingDict (HArr a b) = case (hsingDict a, hsingDict b) of (Dict, Dict) -> Dict 

htypeExpect :: HSing a -> HSing (Expect' a) 
htypeExpect = undefined

fn :: HTypeE (Expect r) -> HTypeE r
fn (HTypeE x) = case hsing' x of 
                  HProb -> HTypeE (unExpect x) 
                  HMeasure a -> case hsingDict (htypeExpect a) of 
                                  Dict -> HTypeE (unExpect x)

-- fn' :: forall r . HTypeE (unExpect r) -> HTypeE r 
fn' :: forall r . HTypeE r -> HTypeE (Expect r) 
fn' (HTypeE x) = case hsing' x of 
                   HMeasure _ -> error "A Measure can't appear in the result of an application of Expect'" 
                   HProb -> HTypeE (Expect x :: Expect r Prob) 


sopExpect :: Embed' r => (Int, [HTypeE (Expect r)]) -> Expect r Void 
sopExpect (c, xs) = Expect (sop'' (c, map fn xs))

unsopExpect :: Embed' r => Expect r Void -> (Int, [HTypeE (Expect r)])
unsopExpect (Expect x) = case unsop'' x of 
                           (c, xs) -> (c, map fn' xs)  



-- caseExpect :: Embed' r => Expect r Void -> [[HTypeE (Expect r)] -> Expect r o] -> Expect r o
-- caseExpect (Expect x) fs = Expect (case'' x (map t fs)) where 

--   t :: Embed' r => ([HTypeE (Expect r)] -> Expect r o) -> ([HTypeE r] -> r (Expect' o))
--   t f xs = undefined


fromVoidExpect :: (Embeddable t, Embed' r) => Expect r Void -> Expect r (HRep t)
fromVoidExpect (Expect x) = Expect (fromVoid x) 

data HTypeE r where 
  HTypeE :: HakaruType t => r t -> HTypeE r 

hsing' :: HakaruType t => r t -> HSing t 
hsing' _ = hsing 

castHType :: forall t r . HakaruType t => HTypeE r -> Maybe (r t) 
castHType (HTypeE t) = case eqHSing (hsing' t) (hsing :: HSing t) of 
                         Just Refl -> Just t 
                         Nothing -> Nothing 

nfnToEx :: SOP.All HakaruType xs => Sing xs -> NFn r o xs -> [HTypeE r] -> r o
nfnToEx SNil (NFn x) [] = x 
nfnToEx s@SCons (NFn f) (x:xs) = do 
  case castHType x of 
    Just x' -> nfnToEx (singTail s) (NFn (f x')) xs 
    Nothing -> error "nfnToEx"
nfnToEx _ _ _ = error "nfnToEx"

nfnsToEx :: SOP.All2 HakaruType xss => Sing xss -> NP (NFn r o) xss -> [[HTypeE r] -> r o]
nfnsToEx = undefined

case''' :: (SingI (Code t), SOP.All2 HakaruType (Code t), Embed' repr, Embeddable t) 
        => repr (HRep t) -> NP (NFn repr o) (Code t) -> repr o
case''' hrep fs = case'' (toVoid hrep) ( nfnsToEx sing fs )






instance Embed (Expect Maple) where 
  sop' p x = 
    case diSing (datatypeInfo p) of 
      Dict -> Expect $ Maple $ unMaple $ sop' p (SOP.unSOP (SOP.hliftA toM (SOP.SOP x)))
        where toM :: Expect Maple a -> Maple a 
              toM (Expect (Maple a)) = Maple a 

  case' p (Expect (Maple x)) fn = 
    case diSing (datatypeInfo p) of
      Dict -> Expect (Maple $ unMaple $ case' p (Maple x) (funMs sing fn))
        where funM :: Sing xs -> NFn (Expect Maple) o xs -> NFn Maple o xs 
              funM SNil (NFn (Expect (Maple f))) = NFn (Maple f)
              funM s@SCons ((NFn f) :: NFn (Expect Maple) o (x ': xs)) = NFn $ \(Maple a) -> 
                let 
                 r :: NFn (Expect Maple) o xs -> NAryFun Maple o xs 
                 r = unFn . funM (singTail s) 
                in r $ NFn $ f $ Expect $ Maple a  

              funMs :: Sing xss -> NP (NFn (Expect Maple) o) xss -> NP (NFn Maple o) xss 
              funMs SNil Nil = Nil
              funMs SCons (a :* as) = funM sing a :* funMs sing as
              funMs _ _ = error "typeError: funMS" 
