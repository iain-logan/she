> {-# OPTIONS_GHC -F -pgmF she #-}

> {-# SHE Aspect #-}

This is a test of code accumulation.

> module Aspect.Foo where

> import Aspect.Bar
> import Aspect.Boo.Doo

> import <- BarCode
> import <- DooCode

> helloWorld = hello ++ ", " ++ world ++ "!"
