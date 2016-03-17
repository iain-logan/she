> {-# OPTIONS_GHC -F -pgmF she #-}

> {-# SHE TypesToKinds, OverrideImps, IdiomBrackets, Superclass #-}

This example covers the following features
 - TypesToKinds
    - Exp pi types
    - Imp pi types
    - Visible forall
    - Deriving SheSingleton
 - OverrideImps
 - Superclass
 - SHE pragma
 
> module TTK.Vec where

> import Data.Foldable
> import Data.Traversable

> data Nat :: * where
>   Z :: Nat
>   S :: Nat -> Nat
>   deriving (Show, Eq, SheSingleton)
 
> data Vec :: Nat -> * -> * where
>   VNil :: Vec Z x
>   (:-) :: x -> Vec n x -> Vec (S n) x

> instance Show x => Show (Vec n x) where
>   show VNil       = "VNil"
>   show (x :- xs)  = show x ++ " :> " ++ show xs

> vtail :: Vec (S n) x -> Vec n x
> vtail (x :- xs) = xs
 
> type family (m :: Nat) :+ (n :: Nat) :: Nat
> type instance Z :+ n = n
> type instance (S m) :+ n = (S) (m :+ n)
 
> vappend :: Vec m x -> Vec n x -> Vec (m :+ n) x
> vappend VNil ys = ys
> vappend (x :- xs) ys = x :- vappend xs ys
> 
> vec :: pi (n). x -> Vec n x
> vec @{Z} x = VNil
> vec @{S n} x = x :- vec @{n} x

> vtake :: pi (n) -> forall (m) -> Vec (n :+ m) x -> Vec (n) x
> vtake {Z} m xs = VNil
> vtake {S n} m @y(x :- xs) = x :- (vtake n m xs)

> instance pi (n :: Nat). Applicative (Vec n) where
>   pure = vec
>   (<*>) = vapp where
>         vapp :: Vec m (s -> t) -> Vec m s -> Vec m t
>         vapp VNil VNil = VNil
>         vapp (f :- fs)  (s :- ss) = f s :- vapp fs ss

> v1 :: Vec (S (S (S Z))) Char
> v1 = pure 'a'
 
> v2 :: Vec (S Z) Char
> v2 = vtake {S Z} (::) v1
 
> v3 :: Vec (S (Z)) Char
> v3 = vec @{(S (Z))} 'a'

> v5 :: Vec (S (Z)) Nat
> v5 = vtake {S (Z)} (:S Z:) (Z :- (Z :- VNil))

> v6 :: Vec (S (Z)) Nat
> v6 = vtake {S (Z)} (:S Z :: Nat:) (Z :- (Z :- VNil))
