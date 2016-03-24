module SHE-Prelude where

record Functor (T : Set -> Set) : Set1 where
  field
    -- OPERATIONS ----------------------------------------------
    map     : forall {X Y} -> (X -> Y) -> T X -> T Y

record Applicative (T : Set -> Set) : Set1 where
  field
    -- OPERATIONS ----------------------------------------------
    pure         : forall {X} -> X -> T X
    _<*>_        : forall {X Y} -> T (X -> Y) -> T X -> T Y

data Zero : Set where

magic : forall {l}{A : Set l} -> Zero -> A
magic ()

record One : Set where
  constructor <>
open One public
{-# COMPILED_DATA One () () #-}
