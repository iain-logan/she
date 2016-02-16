> module Pragma where

> import Data.Maybe
> import Data.List
> import Control.Applicative

> import HaLay
> import Parsley

> data Feature = Aspect | DeBruijn | IdiomBrackets | OverrideImps | TypesToKinds | Superclass
>   deriving (Show, Eq)

> toFeat :: String -> Maybe Feature
> toFeat "Aspect" = Just Aspect
> toFeat "DeBruijn" = Just Aspect
> toFeat "IdiomBrackets" = Just IdiomBrackets
> toFeat "OverrideImps" = Just OverrideImps
> toFeat "TypesToKinds" = Just TypesToKinds
> toFeat "Superclass" = Just Superclass
> toFeat _ = Nothing

> pPragma :: P Tok [Feature]
> pPragma = (mapMaybe toFeat) <$> (spc *> teq (Uid "SHE") *> spc *> feat <* spc <* pRest) where
>    feat = (pSep (spc *> teq (Sym ",") *> spc) uid) <|> pure <$> uid

> features :: [[Tok]] -> [Feature]
> features =  nub . fromLines

> fromLines :: [[Tok]] -> [Feature]
> fromLines (t : ts) = case fromLine t of
>   Just feat -> feat ++ fromLines ts
>   Nothing -> fromLines ts
> fromLines [] = []

> fromLine :: [Tok] -> Maybe [Feature]
> fromLine (Com ('{' : '-' : '#' : pragma) : ts) = do
>   case listToMaybe [foldr (++) [] $ ready "notMattering" pragma] >>= (parse pPragma) of
>     Just feats -> case fromLine ts of
>       Just feats2 -> Just $ feats ++ feats2
>       Nothing -> Just feats
>     Nothing -> fromLine ts
> fromLine (t : ts) = fromLine ts
> fromLine [] = Nothing
