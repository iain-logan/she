> module Main where

> import System.Environment
> import System.FilePath
> import Data.Char
> import Data.Traversable
> import Data.Foldable
> import Data.List
> import Debug.Trace

> import HaLay
> import Imports
> import Aspect
> import DeBruijn
> import TypesToKinds
> import IdiomBrackets
> import Superclass
> import Parsley

> sheGoes :: FilePath -> [[Tok]] -> [[Tok]] -> ([[Tok]], [[Tok]])
> sheGoes mo inh hs0 =
>   let nl = dental hs0
>       hersi0 = getGuts inh
>       (higs0, _) = foldMap higgle hersi0
>       (higs1, hs1) = foldMap higgle hs0
>       higs = higs0 ++ higs1
>       herso0 = higs1 >>= higOut
>       hs2 = piggle higs hs1
>       hs2'5 = deBruijn hs2
>       hs2'75 = map idiomBrackets hs2'5
>       (hs3'5, herso2) = superclass nl hersi0 hs2'75
>       hs5 = addImport $ typesToKinds $ noDerSing $ addSing $ addSingAlone hs3'5
>   in  (inh ++
>        [[NL (mo ++ ".hers", 0)],
>         [KW "module", Spc " ", Uid mo, Spc " ", L "where"
>           (redent [NL (mo ++ ".hers", 0), Spc "  "]
>            (herso0 ++ [[NL (mo ++ ".hers", 0)]]
>                    ++ [[NL (mo ++ ".hers", 0)]] ++ herso2))]]
>       , hs5)

> hsAndHers :: String -> FilePath -> String -> IO (String, String)
> hsAndHers f mo s = do
>   let ihs = ready f s
>   pcs <- storySoFar ihs
>   let (hers, hs) = sheGoes mo pcs ihs
>   return (tokssOut hs, tokssOut hers)

Parameters
x filepath of the original source file (used to create .hers file)
y filepath of the file holding the input (used to locate the hs source, with she sugar in it)
z filepath of the file where she should write its output to

From the haskell documentation about pre processors
"the first argument is the name of the original source file, the second is the
name of the file holding the input, and the third is the name of the file where
cmd should write its output to"

> main :: IO ()
> main = do
>   x : y : z : _ <- getArgs
>   let x' = replaceExtension x ".hers"
>   putStrLn x
>   putStrLn y
>   putStrLn z
>   f <- readFile y
>   (f', h) <- hsAndHers x (takeBaseName x) f
>   writeFile x' h
>   writeFile z f'
