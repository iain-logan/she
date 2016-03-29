> module Pragma where

> import Data.Maybe
> import Data.List
> import Control.Applicative

> import HaLay
> import Parsley

> data Feature = Aspect | DeBruijn | IdiomBrackets | OverrideImps | DependentQuantifiers | Superclass
>   deriving (Show, Eq)

> data FeatureReq = FeatReq Feature [Feature]
>   deriving (Show, Eq)

> reqFeat :: [FeatureReq] -> [Feature]
> reqFeat ((FeatReq feat _) : frs) = feat : (reqFeat frs)
> reqFeat [] = []

> reqSat :: FeatureReq -> [Feature] -> Bool
> reqSat (FeatReq feat (r : reqs)) feats = (elem r feats) && (reqSat (FeatReq feat reqs) feats)
> reqSat (FeatReq _ []) _ = True

> toFeat :: String -> Maybe FeatureReq
> toFeat f
>   | f == show Aspect        = Just $ FeatReq Aspect []
>   | f == show DeBruijn      = Just $ FeatReq DeBruijn []
>   | f == show IdiomBrackets = Just $ FeatReq IdiomBrackets []
>   | f == show OverrideImps  = Just $ FeatReq OverrideImps [DependentQuantifiers]
>   | f == show DependentQuantifiers  = Just $ FeatReq DependentQuantifiers []
>   | f == show Superclass    = Just $ FeatReq Superclass []
>   | otherwise = Nothing

> allFeats :: [FeatureReq]
> allFeats = mapMaybe make [Aspect, DeBruijn, IdiomBrackets,
>                           OverrideImps, DependentQuantifiers, Superclass] where
>   make feat = toFeat (show feat)

> pPragma :: P Tok [FeatureReq]
> pPragma = (mapMaybe toFeat) <$> (spc *> teq (Uid "SHE") *> spc *> feat <* spc <* pRest) where
>    feat = (pSep (spc *> teq (Sym ",") *> spc) uid) <|> pure <$> uid

> features :: [[Tok]] -> [FeatureReq]
> features =  nub . fromLines

> fromLines :: [[Tok]] -> [FeatureReq]
> fromLines (t : ts) = case fromLine t of
>   Just feat -> feat ++ fromLines ts
>   Nothing -> fromLines ts
> fromLines [] = []

> fromLine :: [Tok] -> Maybe [FeatureReq]
> fromLine (Com ('{' : '-' : '#' : pragma) : ts) = do
>   case listToMaybe [foldr (++) [] $ ready "notMattering" pragma] >>= (parse pPragma) of
>     Just feats -> case fromLine ts of
>       Just feats2 -> Just $ feats ++ feats2
>       Nothing -> Just feats
>     Nothing -> fromLine ts
> fromLine (t : ts) = fromLine ts
> fromLine [] = Nothing

> noShePrag :: [[Tok]] -> [[Tok]]
> noShePrag [] = []
> noShePrag (ts : tss) = work ts : noShePrag tss where
>   work (c@(Com ('{' : '-' : '#' : pragma)) : ts) =
>     case listToMaybe [foldr (++) [] $ ready "notMattering" pragma] >>= (parse pPragma) of
>       Just _ -> Com "" : work ts
>       Nothing -> c : work ts
>   work (t : ts) = t : work ts
>   work [] = []
