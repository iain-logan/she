> module Main where

> import System.Environment
> import System.FilePath
> import Data.Char
> import Data.Traversable
> import Data.Foldable
> import Data.List
> import Debug.Trace
> import System.Exit

> import HaLay
> import Pragma
> import Imports
> import Aspect
> import DeBruijn
> import TypesToKinds
> import IdiomBrackets
> import Superclass
> import Parsley
> import Chase

> aspect :: [[Tok]] -> [[Tok]] -> ([[Tok]], [[Tok]])
> aspect hs0 hersi0 = (herso0, hs2) where
>   (higs0, _) = foldMap higgle hersi0
>   (higs1, hs1) = foldMap higgle hs0
>   higs = higs0 ++ higs1
>   herso0 = higs1 >>= higOut
>   hs2 = piggle higs hs1

> sheGoes :: FilePath -> [[Tok]] -> [[Tok]] -> [FeatureReq] -> ([[Tok]], [[Tok]])
> sheGoes mo inh hs0'1 feats =
>   let hs0 = addExtens hs0'1
>       hersi0 = getGuts inh
>       (herso0, hs2) =
>                if elem Aspect        $ reqFeat feats then aspect hs0 hersi0 else ([], hs0)
>       hs2'5  = if elem DeBruijn      $ reqFeat feats then deBruijn hs2 else hs2
>       hs2'75 = if elem IdiomBrackets $ reqFeat feats then map idiomBrackets hs2'5 else hs2'5
>       hs4    = if elem OverrideImps  $ reqFeat feats then overImp hs2'75 else hs2'75
>       hs5    = if elem TypesToKinds  $ reqFeat feats then
>                   addImport $ typesToKinds $ noDerSing $ addSing $ addSingAlone hs4
>                else hs4
>       (hs6, herso2) = if elem Superclass $ reqFeat feats then
>                         superclass (dental hs0) hersi0 hs5
>                       else (hs5, [])
>   in  (inh ++
>        [[NL (mo ++ ".hers", 0)],
>         [KW "module", Spc " ", Uid mo, Spc " ", L "where"
>           (redent [NL (mo ++ ".hers", 0), Spc "  "]
>            (herso0 ++ [[NL (mo ++ ".hers", 0)]]
>                    ++ [[NL (mo ++ ".hers", 0)]] ++ herso2))]]
>       , hs6)

> hsAndHers :: String -> FilePath -> String -> IO (String, String)
> hsAndHers f mo s = do
>   let ihs = ready f s
>   toChase <- imports (takeDirectory f) ihs
>   if (0 /= (length $ filter ((f ==) . fst) toChase)) then
>     do
>       putStrLn $ "SHE detects that " ++ f ++ " contains a circular dependency!"
>       exitFailure
>     else do
>       sequence $ map (\ (fp, f) -> sheStartsNoReadJstHers fp f) toChase
>       let selectedFeats = features ihs
>       let feats = if null selectedFeats then allFeats else selectedFeats
>       pcs <- storySoFar ihs
>       if (not $ foldr (\ fr suc -> (reqSat fr $ reqFeat feats) && suc) True feats) then
>         do
>           putStrLn "Could not satisfy all feature requirments."
>           putStrLn "An enabled feature depends on a disabled feature."
>           exitFailure
>         else do
>           let (hers, hs) = sheGoes mo pcs (noShePrag ihs) feats
>           return (tokssOut hs, tokssOut hers)

> sheStartsNoReadJstHers :: FilePath -> String -> IO ()
> sheStartsNoReadJstHers x f = do
>   let x' = replaceExtension x ".hers"
>   (f', h) <- hsAndHers x (takeBaseName x) f
>   writeFile x' h

> sheStartsNoRead :: FilePath -> String -> FilePath -> IO ()
> sheStartsNoRead x f z = do
>   let x' = replaceExtension x ".hers"
>   (f', h) <- hsAndHers x (takeBaseName x) f
>   writeFile x' h
>   writeFile z f'

> sheStarts :: FilePath -> FilePath -> FilePath -> IO ()
> sheStarts x y z = do
>   putStrLn x
>   putStrLn y
>   putStrLn z
>   f <- readFile y
>   sheStartsNoRead x f z

Parameters
x filepath of the original source file (used to create .hers file)
y filepath of the file holding the input (used to locate the hs source, with she sugar in it)
z filepath of the file where she should write its output to

> main :: IO ExitCode
> main = do
>   args <- getArgs
>   case args of
>     x : y : z : [] -> do
>       sheStarts x y z
>       exitSuccess
>     _ -> do putStrLn "She is a Haskell preprocessor."
>             putStrLn "It is recomended that you let GHC invoke SHE itself."
>             putStrLn "To achieve this, add \"{-# OPTIONS_GHC -F -pgmF she #-}\" to the top of your source file."
>             exitWith $ ExitFailure 2

