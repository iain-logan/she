> {-# OPTIONS_GHC -F -pgmF she #-}

> module Main where

> import Aspect.Foo
> import TTK.Vec
> import Idiom.Vec

> main :: IO ()
> main = do
>   putStrLn "It all compiled!"
>   putStrLn "This gives us some confidence that nothing is broken."
