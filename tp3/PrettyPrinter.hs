module PrettyPrinter (
       printTerm,     -- pretty printer para terminos
       printType,     -- pretty printer para tipos
       )
       where

import Common
import Text.PrettyPrint.HughesPJ

-- lista de posibles nombres para variables
vars :: [String]
vars = [ c : n | n <- "" : map show [(1::Integer)..], c <- ['x','y','z'] ++ ['a'..'w'] ]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos

pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k)         = text (vs !! (ii - k - 1))
pp _  _  (Free (Global s)) = text s
pp ii vs (i :@: c) = sep [parensIf (isLam i) (pp ii vs i),
                          nest 1 (parensIf (isLam c || isApp c) (pp ii vs c))]
pp ii vs (Lam t c) = text "\\" <>
                     text (vs !! ii) <>
                     text ":" <>
                     printType t <>
                     text ". " <>
                     pp (ii+1) vs c
pp ii vs (Let t1 t2) = sep [text "let " <> text (vs !! ii) <>
                       text " = " <> parens (pp (ii+1) vs t1),
                       text " in " <> parens (pp (ii+1) vs t2)]
pp ii vs (As u t) = parens $ pp ii vs u <>
                    text "as" <>
                    printType t
pp ii vs (TUnit) = text "unit"
pp ii vs (TPair t1 t2) = text "(" <> pp ii vs t1 <>
                         text ", " <> pp ii vs t2 <>
                         text ")"
pp ii vs (Fst t) = text "fst " <> pp ii vs t
pp ii vs (Snd t) = text "snd " <> pp ii vs t

isLam :: Term -> Bool
isLam (Lam _ _) = True
isLam  _      = False

isApp :: Term -> Bool
isApp (_ :@: _) = True
isApp _         = False

-- pretty-printer de tipos
printType :: Type -> Doc
printType Base         = text "B"
printType (Fun t1 t2)  = sep [ parensIf (isFun t1) (printType t1),
                               text "->",
                               printType t2]
printType Unit         = text "Unit"
printType (Pair t1 t2) = sep [ text "(" <> printType t1, text ", ",
                               printType t2 <> text ")" ]

isFun :: Type -> Bool
isFun (Fun _ _)        = True
isFun _                = False

fv :: Term -> [String]
fv (Bound _)         = []
fv (Free (Global n)) = [n]
fv (t :@: u)         = fv t ++ fv u
fv (Lam _ u)         = fv u
fv (Let u u')        = fv u ++ fv u'
fv (As u _)          = fv u
fv (TUnit)           = []
fv (TPair u u')      = fv u ++ fv u'
fv (Fst u)           = fv u
fv (Snd u)           = fv u

---
printTerm :: Term -> Doc
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t

