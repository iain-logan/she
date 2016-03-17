> {-# OPTIONS_GHC -F -pgmF she #-}

> {-# SHE Aspect #-}

Here we test exporting and importing from the same directory.

> module Aspect.Bar where

> import -> BarCode where
>   world = "World"
