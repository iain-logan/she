> module Main where

> import Aspect.Foo
> import TTK.Vec
> import Idiom.Vec
> import DeBruijn.Binding

> main :: IO ()
> main = do
>   putStrLn helloWorld
>   putStrLn "It all compiled!"
>   putStrLn "This gives us some confidence that nothing is broken."
