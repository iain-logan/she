> {-# OPTIONS_GHC -F -pgmF she #-}

> {-# SHE IdiomBrackets #-}

> module Idiom.Vec where

> import TTK.Vec

> idiomV1 = (| (\ x y z -> x + y + z) v1 v1 v1|) where
>   v1 = 4 :- (2 :- (3 :- VNil))
