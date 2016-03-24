
module Vec where

open import SHE-Prelude

data Nat : Set where
  z : Nat
  s : Nat -> Nat

data Vec (X : Set) : Nat -> Set where
  vNil : Vec X z
  _:-_ : forall {n} ->
         X -> Vec X n -> Vec X (s n)

_+N_ : Nat -> Nat -> Nat
z  +N n = n
s m +N n = s (m +N n)

_vappend_ : forall {X m n} -> Vec X m -> Vec X n -> Vec X (m +N n)
vNil vappend ys = ys
(x :- xs) vappend ys = x :- (xs vappend ys)

vec : forall {n X} -> X -> Vec X n
vec {z} x = vNil
vec {s n} x = x :- vec x

_N>=_ : Nat -> Nat -> Set
m N>= z = One
z N>= s n  = Zero 
s m N>= s n = m N>= n

vtake : {X : Set}{m : Nat}(n : Nat) -> m N>= n -> Vec X m -> Vec X n
vtake (s n) () vNil
vtake z p xs = vNil
vtake (s n) p (x :- xs) = x :- (vtake n p xs)

VecApp : forall {n} -> Applicative \X -> Vec X n
VecApp {n} = record
  { pure  = vec
  ; _<*>_ = vapp
  } where
    vapp : forall {n X Y} ->
       Vec (X -> Y) n -> Vec X n -> Vec Y n
    vapp vNil vNil = vNil
    vapp (f :- fs) (x :- xs) = f x :- vapp fs xs

v1 : Vec Nat (s (s (s z)))
v1 = Applicative.pure VecApp z
 
v2 : Vec Nat (s z)
v2 = vtake (s z) <> v1
 
v3 : Vec Nat (s z)
v3 = vec {s z} z

v5 : Vec Nat (s z)
v5 = vtake (s z) <> (z :- (z :- vNil))

v6 : Vec Nat (s z)
v6 = vtake (s z) <> (z :- (z :- vNil))
