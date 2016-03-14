> module Chase where

> import Parsley
> import HaLay
> import Data.Maybe
> import Control.Applicative
> import Control.Monad
> import Control.Exception
> import System.FilePath
> import System.Directory
> import Distribution.Simple.PreProcess.Unlit
> import System.IO.Error

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
>   f <- tryJust (guard . isDoesNotExistError) $ readFile pathHs
>   case f of
>     Right f -> do
>       isOld <- isOutDate pathHs
>       return $ if isOld then Just $ (pathHs, f) else Nothing
>     Left e  -> do
>       let pathLhs = path ++ ".lhs"
>       f <- tryJust (guard . isDoesNotExistError) $ readFile pathLhs
>       case f of
>         Right f -> case unlit pathLhs f of
>           Left f  -> do
>             isOld <- isOutDate pathLhs
>             return $ if isOld then Just $ (pathLhs, f) else Nothing
>           Right e -> fail e
>         Left e  -> return Nothing

> isOutDate :: FilePath -> IO Bool
> isOutDate fps = do
>   let fph = replaceExtension fps ".hers"
>   srcT <- getModificationTime fps
>   herT <- tryJust (guard . isDoesNotExistError) (getModificationTime fph)
>   case herT of
>     Left e -> return True
>     Right herT -> return $ srcT > herT

