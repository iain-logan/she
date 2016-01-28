> module TypesToKinds where

> import Data.List
> import Data.Maybe
> import Control.Applicative

> import HaLay
> import Parsley

> singPrefix :: String
> singPrefix = "S"

> dataGrok :: [Tok] -> [[Tok]]
> dataGrok cs@(KW "newtype" : T Ty _ : ds)
>    = map blat (fillet ds)
> dataGrok cs@(KW "data" : T Ty _ : ds)
>    = map blat (fillet ds)
> dataGrok cs = []

> blat :: (Tok, Int) -> [Tok]
> blat (c, j) =
>     [KW "data", T Ty (cxs id ++ [Spc " "]), Sym "="] ++ cxs ty where
>   cxs f = Spc " " : c :
>           ([1 .. j] >>= (\k -> [Spc " ", f (Lid ("x" ++ show k))]))
>   ty t = T Ty [t]

> fillet :: [Tok] -> [(Tok, Int)]
> fillet [] = []
> fillet (Sym "=" : cs) =
>   case parse (pSep (spc *> teq (Sym "|")) (spc *> pOldSyn)) cs of
>     Just sis -> sis
>     _ -> []
> fillet (L "where" css : _)     = css >>= gadtSyn
> fillet (_ : cs) = fillet cs

> pOldSyn :: P Tok (Tok, Int)
> pOldSyn = (\s -> (jig s, 2)) <$ pArg <* spc <*> infC <* pArg
>       <|> (,) <$> pCId
>               <* spc
>               <*> (pBr Crl pFields <|> length <$> many pArg)
>               <* spc

> jig :: String -> Tok
> jig s = (B Rnd [Sym (":$#$#$#" ++ s)])

> pArg :: P Tok ()
> pArg = spc <* (
>   (() <$ pTag Ty pRest) <|>
>   teq (Sym "!") *> pArg
>   )

> pFields :: P Tok Int
> pFields = 0 <$ pEnd
>       <|> (1 +) <$ lid <*> pFields  -- right, assuming types are chunked
>       <|> next *> pFields

> gadtSyn :: [Tok] -> [(Tok, Int)]
> gadtSyn cs = case parse pGDecl cs of
>   Just (ss, i) -> map (flip (,) i) ss
>   _ -> []

> pGDecl :: P Tok ([Tok], Int)
> pGDecl = (,) <$> pSep (spc *> teq (Sym ",")) pCId <* spc <* teq (Sym "::")
>              <*> pTag Ty pArity <* pRest

> pCId :: P Tok Tok
> pCId = Uid <$> (("SheTy" ++) <$> uid)
>    <|> jig <$> pBr Rnd (spc *> infC <* spc)

> pArity :: P Tok Int
> pArity = 0 <$ pEnd
>      <|> (1 +) <$ teq (Sym "->") <*> pArity
>      <|> next *> pArity

> piB :: P Tok (String, [Tok])
> piB = (,) <$ spc <*> lid <* spc <* teq (Sym "::") <* spc <*> pRest

> pPiExp :: P Tok ([(String, [Tok])], [Tok])
> pPiExp = (,)  <$> some (spc *> pBr Rnd piB) <* spc <* teq (Sym "->")
>              <*> pRest
>              where

> pPiImp :: P Tok ([(String, [Tok])], [Tok])
> pPiImp = (,)  <$> some (spc *> pBr Rnd piB) <* spc <* teq (Sym ".")
>              <*> pRest

> pProxyRequest :: P Tok ([Tok], [Tok])
> pProxyRequest = (,) <$> some (tok (/= Sym "::")) <* teq (Sym "::") <* spc
>                     <*> pTag Ty (some (tok (/= Sym ":")) <* teq (Sym ":"))

> proxyRequest :: [Tok] -> [Tok] -> Tok
> proxyRequest tm ty = B Rnd [
>       Lid "sheTypes", Spc " ", B Rnd [
>         Uid "SheProxy", Spc " ", Sym "::", T Ty [Spc " ",
>           Uid "SheProxy", Spc " ", B Rnd ty, Spc " ", B Rnd (munge exTTK tm)
>       ]]]

> exTTK :: [Tok] -> Maybe [Tok]
> exTTK (B Sqr us : ts) = Just $ mkL (munge exTTK us) : munge exTTK ts where
>   mkL [] = Uid "SheSpecialNil"
>   mkL ts = case span (/= Sym ",") ts of
>     (ss, []) ->
>       B Rnd [B Rnd ss, Spc " ", Sym ":$#$#$#:", Spc " ", Uid "SheSpecialNil"]
>     (ss, _ : ts) ->
>       B Rnd [B Rnd ss, Spc " ", Sym ":$#$#$#:", Spc " ", mkL ts]
> exTTK (Uid s : ts) = Just $ Uid ("SheTy" ++ s) : munge exTTK ts
> exTTK (Sym (':' : s) : ts) = Just $ Sym (":$#$#$#:" ++ s) : munge exTTK ts
> exTTK _ = Nothing

> witMu :: [Tok] -> Maybe [Tok]
> witMu (B Sqr us : ts) = Nothing -- not sure when we have square brackets in curly
> witMu (Uid s : ts) = Just $ Uid (singPrefix ++ s) : munge witMu ts
> witMu (Sym (':' : s) : ts) = Just $ Sym (':' : '%' : s) : munge witMu ts
> witMu _ = Nothing

> tyTTK :: [Tok] -> Maybe [Tok]
> tyTTK (B Crl [T Ex (Sym ":" : us)] : ts) = case parse pProxyRequest us of
>   Just (tm, ty) -> Just $
>     [Uid "SingI", Spc " ", B Rnd (tm ++ (Spc " " : Sym "::" : Spc " " : []) ++ ty)]
>     ++ munge tyTTK ts
>   _ -> Nothing
> tyTTK (Lid "pi" : ts) = case parse pPiExp ts of
>   Just pr -> Just (pityex pr)
>   Nothing -> case parse pPiImp ts of
>     Just pr -> Just (pityim pr)
>     Nothing -> Nothing
>   where
>   pityex :: ([(String, [Tok])], [Tok]) -> [Tok]
>   pityex (xss, ts) =
>     [KW "forall", Spc " "] ++
>     (xss >>= \ (x, _) -> [Lid x, Spc " "]) ++
>     [Sym ".", Spc " "] ++
>     (xss >>= \ (x, ss) ->
>       [Uid "Sing", Spc " ", B Rnd (Lid x : Spc " " : Sym "::" : Spc " " : B Rnd (munge tyTTK ss) : []),
>        Spc " ", Sym "->", Spc " "]) ++
>     munge tyTTK ts
>   pityim :: ([(String, [Tok])], [Tok]) -> [Tok]
>   pityim (xss, ts) =
>     [KW "forall", Spc " "] ++
>     (xss >>= \ (x, _) -> [Lid x, Spc " "]) ++
>     [Sym ".", Spc " "] ++
>     (xss >>= \ (x, ss) ->
>       [Uid "SingI", Spc " ", B Rnd (Lid x : Spc " " : Sym "::" : Spc " " : B Rnd (munge tyTTK ss) : []),
>        Spc " ", Sym "=>", Spc " "]) ++
>     munge tyTTK ts
> tyTTK _ = Nothing

> ttkMu :: [Tok] -> Maybe [Tok]
> ttkMu (T Ty us : ts) = Just $ T Ty (munge tyTTK us) : munge ttkMu ts
> ttkMu (B Rnd (Sym ":" : us) : ts) = case parse pProxyRequest us of
>  -- Just (tm, ty) -> Just $ proxyRequest tm ty : munge ttkMu ts
>   _ -> Nothing
> ttkMu (B Crl us : ts)
>   | piArg us = Just $ B Rnd (munge witMu us) : munge ttkMu ts
>   where
>     piArg xs | elem (Sym "=") xs = False
>     piArg xs | elem (Sym "::") xs = False
>     piArg _ = True
> ttkMu _ = Nothing

> pModule :: P Tok ()
> pModule = teq (KW "module") *> spc
>   *> uid *> spc *> teq (L "where" []) *> pEnd

> singImport :: [[Tok]]
> singImport =
>   [[KW "import",
>     Spc " ",
>     Uid "Data",
>     Sym ".",
>     Uid "Singletons",
>     Sym ".",
>     Uid "TH"],
>     [],
>     [line]
>    ] where
>   line = NL ("Dunno.lhs", 0)

> addImport :: [[Tok]] -> [[Tok]]
> addImport ls = case span (\ (p, _) -> Nothing == p) (map (\ l -> (parse pModule l, l)) ls) of
>   (bls, (il : (_, []) : (_, nl@(NL (f, l) : nls)) : als)) ->
>     (map (snd) bls) ++ (snd il : [] : nl : ((redent nl singImport) ++ (map (snd) als)))
>   _ -> ls

> addSing :: [[Tok]] -> [[Tok]]
> addSing ls = map (\ l -> addSing' (parse pGADT l, l)) ls where
>   addSing' (Just ((s, i), (cs, ds)), l) | elem "SheSingleton" ds =
>     [Sym "$", B Rnd [Lid "singletons", Spc " ",
>                      B Sqr (Sym "d": Sym "|" :
>                             NL ("Dunno.lhs", 0) :
>                             (indent l) ++
>                             [NL ("Dunno.lhs", 0), Spc "  ", Sym "|"])],
>      NL ("Dunno.lhs", 0)]
>   addSing' (_, l) = l

> addSingAlone :: [[Tok]] -> [[Tok]]
> addSingAlone ls = map (\ l -> fromMaybe l ((genSing) <$> (parse pSingAlone l))) ls where
>   genSing :: String -> [Tok]
>   genSing s = [Sym "$", B Rnd ([Lid "genSingletons", Spc " ", B Sqr (Sym "''" : Uid s : [])])]

> pSingAlone :: P Tok String
> pSingAlone = spc *> teq (KW "deriving") *> spc *> teq (KW "instance") *>
>   pTag Ty ( spc *> teq (Uid "SheSingleton") *> spc *> uid <* spc) <* pEnd

> indent :: [Tok] -> [Tok]
> indent ts = (Spc "  ") : (indent' ts) where
>   indent' (nl@(NL (f, l)) : ts) = nl : (Sym "  ") : (indent' ts)
>   indent' (B b ss : ts) = B b (indent' ss) : indent' ts
>   indent' (L k sss : ts) = L k (map (indent') sss) : indent' ts
>   indent' (T t ss : ts) = T t (indent' ss) : indent' ts
>   indent' (t : ts) = t : (indent' ts)
>   indent' [] = []

> typesToKinds :: [[Tok]] -> [[Tok]]
> typesToKinds = map (munge ttkMu)

> data ConTy = ConTy
>   { cName     :: String
>   , cForall   :: [Tok]
>   , cInst     :: [Tok]
>   , cArgs     :: [[Tok]]
>   , cFam      :: [Tok]
>   , cIndices  :: [Tok]
>   } deriving (Show)

> noDerSing :: [[Tok]] -> [[Tok]]
> noDerSing = map (munge ndsMu) where
>   ndsMu ts = case parse pDeriving ts of
>     Just xs -> Just $ mkDer (filter (/= "SheSingleton") xs)
>     _ -> Nothing
>   mkDer [] = []
>   mkDer [x] = [KW "deriving", Spc " ", Uid x]
>   mkDer (x : xs) =
>     [KW "deriving", Spc " ",
>      B Rnd (Uid x : (xs >>= \x -> [Sym ",", Spc " ", Uid x]))]

> pGADT :: P Tok ((String, Int), ([ConTy], [String]))
> pGADT = (,)
>   <$   tok (`elem` [KW "data", KW "newtype"]) <* spc
>   <*>  pTag Ty ((,) <$ spc <*> uid <* spc <*>  pGArity <* pRest) <* spc
>   <*>  pLay "where" ((,) <$> pGCons <*> pDer)
>   <*   pRest
>   where
>     pGArity = length <$> many (lid <* spc)
>               <|> (teq (Sym "::") *> spc *> pTag Ki pArity)
>     pGCons = tok (all isSpcT) *> pGCons
>              <|> (:) <$> grok (parse pGConTy) next <*> pGCons
>              <|> pure []
>     pDer = grok (parse pDeriving) next <* pRest <|> pure []

> pGConTy :: P Tok ConTy
> pGConTy = (ConTy <$ spc <*> uid <* spc <* teq (Sym "::") <* spc) >>= \f ->
>           pTag Ty (f <$> pFA <*> pCI <*> many pARG <* spc <*> (((:[]). Uid) <$> uid)
>                    <*> pRest)
>   where
>     pFA = spc *> (
>             teq (KW "forall") *> some (tok (/= Sym ".")) <* teq (Sym ".")
>             <|> [] <$ spc)
>     pCI = some (tok (/= Sym "=>")) <* teq (Sym "=>")
>           <|> [] <$ spc
>     pARG = some (tok (/= Sym "->")) <* teq (Sym "->")


> pDeriving :: P Tok [String]
> pDeriving = teq (KW "deriving") *> spc *>
>             (pure <$> uid <|>
>             pBr Rnd (spc *> pSep (spc *> teq (Sym ",") *> spc) uid <* spc))
