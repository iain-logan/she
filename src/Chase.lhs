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
> import Data.Foldable
> import System.Exit

> type Import = [String]
> type Module = [String]

> pPath :: P Tok [String]
> pPath = (pSep (teq (Sym ".")) uid) <|> pure <$> uid

> pModule :: P Tok Module
> pModule = reverse <$> (spc *> teq (KW "module") *> spc *> pPath <* spc <* teq (L "where" []) <* pRest)

> pImport :: P Tok Import
> pImport = (spc *> teq (KW "import") *> spc *> pPath <* spc <* pEnd) where

> imports :: FilePath -> [[Tok]] -> IO [(FilePath, String)]
> imports cur [] = return []
> imports cur (ts : tts) = case parse pModule ts of
>   Nothing -> imports cur tts
>   Just ms -> imports' cur tts ms where
>     imports' :: FilePath -> [[Tok]] -> Module -> IO [(FilePath, String)]
>     imports' cur [] _ = return []
>     imports' cur (ts : tts) ms = let tail = imports' cur tts ms in
>       case (parse pImport ts) of
>         Just is -> isLocal cur is ms >>=
>           maybe tail (\ fp -> tail >>= (\ fps -> return $ fp : fps))
>         Nothing -> tail

> backUpPath :: FilePath -> Module -> FilePath
> backUpPath pre (m : []) = pre
> backUpPath pre (m : ms) = backUpPath (takeDirectory pre) ms
> backUpPath pre _ = pre

> isLocal :: FilePath -> Import -> Module -> IO (Maybe (FilePath, String))
> isLocal curAll imps ms = do
>   let cur = backUpPath curAll ms
>   let path = foldl' (\ pb -> ((pb ++ "/") ++ )) cur imps
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
>           Right e -> do
>             putStrLn e
>             exitFailure
>         Left e  -> return Nothing

> isOutDate :: FilePath -> IO Bool
> isOutDate fps = do
>   let fph = replaceExtension fps ".hers"
>   srcT <- getModificationTime fps
>   herT <- tryJust (guard . isDoesNotExistError) (getModificationTime fph)
>   case herT of
>     Left e -> return True
>     Right herT -> return $ srcT > herT

