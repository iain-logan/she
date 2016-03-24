{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TypeFamilies, GADTs, KindSignatures, DataKinds, PolyKinds, TypeOperators, FlexibleContexts, RankNTypes, UndecidableInstances, FlexibleInstances, InstanceSigs, DefaultSignatures #-}

module Vec where

import Data.Singletons.TH
import Data.Proxy

data Nat :: * where
  Z :: Nat
  S :: Nat -> Nat
  deriving (Show, Eq)
$(genSingletons [''Nat])

data Vec :: Nat -> * -> * where
  VNil :: Vec Z x
  (:-) :: x -> Vec n x -> Vec (S n) x

instance Show x => Show (Vec n x) where
  show VNil       = "VNil"
  show (x :- xs)  = show x ++ " :> " ++ show xs

type family (m :: Nat) :+ (n :: Nat) :: Nat
type instance Z :+ n = n
type instance (S m) :+ n = (S) (m :+ n)

vappend :: Vec m x -> Vec n x -> Vec (m :+ n) x
vappend VNil ys = ys
vappend (x :- xs) ys = x :- vappend xs ys

vec :: SingI n => x -> Vec n x
vec a = vecWorker sing a

vecWorker :: Sing n -> x -> Vec n x
vecWorker SZ x = VNil
vecWorker (SS n) x = x :- vecWorker (n) x

vtake :: Sing n -> Proxy m -> Vec (n :+ m) x -> Vec n x
vtake SZ m xs = VNil
vtake (SS n) m (x :- xs) = x :- (vtake n m xs)

instance SingI n => Applicative (Vec n) where
  pure = vec
  (<*>) = vapp where
         vapp :: Vec m (s -> t) -> Vec m s -> Vec m t
         vapp VNil VNil = VNil
         vapp (f :- fs)  (s :- ss) = f s :- vapp fs ss

instance SingI n => Functor (Vec n) where
  fmap = (<*>) . pure

v1 :: Vec (S (S (S Z))) Char
v1 = pure 'a'
 
v2 :: Vec (S Z) Char
v2 = vtake (SS SZ) (Proxy) v1

v3 :: Vec (S Z) Char
v3 = vecWorker (SS SZ) 'a'

v5 :: Vec (S Z) Nat
v5 = vtake (SS SZ) (Proxy :: Proxy (S Z)) (Z :- (Z :- VNil))

v6 :: Vec (S Z) Nat
v6 = vtake (SS SZ) (Proxy :: Proxy (S Z :: Nat)) (Z :- (Z :- VNil))

