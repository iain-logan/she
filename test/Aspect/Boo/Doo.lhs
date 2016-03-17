> {-# OPTIONS_GHC -F -pgmF she #-}

> {-# SHE Aspect #-}

Here we test exporting and importing from a nested directory.

> module Aspect.Boo.Doo where

> import -> DooCode where
>   hello = "Hello"
