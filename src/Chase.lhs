> module Chase where

> import Parsley
> import HaLay
> import Data.Maybe
> import Control.Applicative
> import System.FilePath
> import System.Directory
> import Distribution.Simple.PreProcess.Unlit

> data Import = Import [String]
>   deriving Show

> pImport :: P Tok Import
> pImport = Import <$> (spc *> teq (KW "import") *> spc *> path <* spc <* pEnd) where
>   path = (pSep (teq (Sym ".")) uid) <|> pure <$> uid

> imports :: FilePath -> [[Tok]] -> IO [(FilePath, String)]
> imports cur [] = return []
> imports cur (ts : tts) = let tail = imports cur tts in
>   case parse pImport ts of
>     Just is -> isLocal cur is >>=
>       maybe tail (\ fp -> tail >>= (\ fps -> return $ fp : fps))
>     Nothing -> tail where

> isLocal :: FilePath -> Import -> IO (Maybe (FilePath, String))
> isLocal cur (Import imps) = do
>   let path = foldr (\ pb -> (++ ('/' : pb))) cur imps
>   let pathHs = path ++ ".hs"
>   isHs <- doesFileExist $ pathHs
>   case isHs of
>     True -> do
>       f <- readFile pathHs
>       return $ Just $ (pathHs, f)
>     False -> do
>       let pathLhs = path ++ ".lhs"
>       isLhs <- doesFileExist $ pathLhs
>       case isLhs of
>         True -> do
>           f <- readFile pathLhs
>           case unlit pathLhs f of
>             Left  f -> return $ Just $ (pathLhs, f)
>             Right e -> fail e
>         False -> return Nothing
