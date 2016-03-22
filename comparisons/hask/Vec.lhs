> {-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TypeFamilies, GADTs, KindSignatures, DataKinds, PolyKinds, TypeOperators, FlexibleContexts, RankNTypes, UndecidableInstances, FlexibleInstances, InstanceSigs, DefaultSignatures #-}

> module Vec where

> import Data.Proxy

> data Nat :: * where
>   Z :: Nat
>   S :: Nat -> Nat
>   deriving (Show, Eq)

> data NNat :: Nat -> * where
>   SZ :: NNat Z
>   SS :: NNat n -> NNat (S n)

> class NNAT (n :: Nat) where
>   natty :: NNat n

> instance NNAT Z where
>   natty = SZ

> instance NNAT n => NNAT (S n) where
>   natty = SS natty

> data Vec :: Nat -> * -> * where
>   VNil :: Vec Z x
>   (:-) :: x -> Vec n x -> Vec (S n) x

> instance Show x => Show (Vec n x) where
>   show VNil       = "VNil"
>   show (x :- xs)  = show x ++ " :> " ++ show xs

> type family (m :: Nat) :+ (n :: Nat) :: Nat
> type instance Z :+ n = n
> type instance (S m) :+ n = (S) (m :+ n)

> vappend :: Vec m x -> Vec n x -> Vec (m :+ n) x
> vappend VNil ys = ys
> vappend (x :- xs) ys = x :- vappend xs ys

> vec :: NNAT n => x -> Vec n x
> vec a = vecWorker natty a

> vecWorker :: NNat n -> x -> Vec n x
> vecWorker SZ x = VNil
> vecWorker (SS n) x = x :- (vecWorker n x)

> vtake :: NNat n -> Proxy m -> Vec (n :+ m) x -> Vec n x
> vtake SZ m xs = VNil
> vtake (SS n) m (x :- xs) = x :- (vtake n m xs)

> instance NNAT n => Applicative (Vec n) where
>   pure = vec
>   (<*>) = vapp where
>          vapp :: Vec m (s -> t) -> Vec m s -> Vec m t
>          vapp VNil VNil = VNil
>          vapp (f :- fs)  (s :- ss) = f s :- vapp fs ss

> instance NNAT n => Functor (Vec n) where
>   fmap = (<*>) . pure

> v1 :: Vec (S (S (S Z))) Char
> v1 = pure 'a'
> 
> v2 :: Vec (S Z) Char
> v2 = vtake (SS SZ) (Proxy) v1

> v3 :: Vec (S Z) Char
> v3 = vecWorker (SS SZ) 'a'

> v5 :: Vec (S Z) Nat
> v5 = vtake (SS SZ) (Proxy :: Proxy (S Z)) (Z :- (Z :- VNil))

> v6 :: Vec (S Z) Nat
> v6 = vtake (SS SZ) (Proxy :: Proxy (S Z :: Nat)) (Z :- (Z :- VNil))
