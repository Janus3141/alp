module Simplytyped (
       conversion,    -- conversion a terminos localmente sin nombre
       eval,          -- evaluador
       infer,         -- inferidor de tipos
       quote          -- valores -> terminos
       )
       where

import Data.List
import Data.Maybe
import Prelude hiding ((>>=))
import Text.PrettyPrint.HughesPJ (render)
import PrettyPrinter
import Common

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion' []

conversion' :: [String] -> LamTerm -> Term
conversion' b (LVar n)       = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b (App t u)      = conversion' b t :@: conversion' b u
conversion' b (Abs n t u)    = Lam t (conversion' (n:b) u)
conversion' b (LtLet n t t') = Let (conversion' b t) (conversion' (n:b) t')
conversion' b (LtAs l t)     = As (conversion' b l) t
conversion' _ (LtUnit)       = TUnit
conversion' b (LtPair t u)   = TPair (conversion' b t) (conversion' b u)
conversion' b (LtFst t)      = Fst (conversion' b t)
conversion' b (LtSnd t)      = Snd (conversion' b t)

-----------------------
--- eval
-----------------------

sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n)              = Free n
sub i t (u :@: v)             = sub i t u :@: sub i t v
sub i t (Lam t' u)            = Lam t' (sub (i+1) t u)
sub i t (Let t1 t2)           = Let (sub i t t1) (sub (i+1) t t2) -- Aumentar i en sub t1?
sub i t (As u ty)             = As (sub i t u) ty
sub _ _ (TUnit)               = TUnit
sub i t (TPair u v)           = TPair (sub i t u) (sub i t v)
sub i t (Fst u)               = Fst (sub i t u)
sub i t (Snd u)               = Snd (sub i t u)


-- evaluador de términos
eval :: NameEnv Value Type -> Term -> Value
eval _ (Bound _)             = error "variable ligada inesperada en eval"
eval e (Free n)              = fst $ fromJust $ lookup n e
eval _ (Lam t u)             = VLam t u
eval e (Lam _ u :@: Lam s v) = eval e (sub 0 (Lam s v) u)
eval e (Lam _ u :@: TUnit)   = eval e (sub 0 (TUnit) u)
eval e (Lam t u :@: v)       = case eval e v of
                 VLam t' u' -> eval e (Lam t u :@: Lam t' u')
                 VUnit      -> eval e (Lam t u :@: TUnit)
                 _          -> error "Error de tipo en run-time, verificar type checker"
eval e (u :@: v)             = case eval e u of
                 VLam t u' -> eval e (Lam t u' :@: v)
                 VUnit     -> -- Si hago 'eval e (VUnit :@: v)' no llega a un valor, que hacer?
                 _         -> error "Error de tipo en run-time, verificar type checker"
eval e (Let u u')            = case eval e u of
                 VLam t s  -> eval e (sub 0 (Lam t s) u')
                 VUnit     -> eval e (sub 0 (TUnit) u')
                 _         -> error "Error de tipo en run-time, verificar type checker"
eval e (As u _)             = eval e u
eval _ (TUnit)               = VUnit
eval e (TPair u v)           = let u' = eval e u
                               in VPair u' (eval e v)
eval e (Fst t)               = case eval e t of
                 VPair v _ -> v
                 _         -> error "Error de tipo en run-time, verificar type checker"
eval e (Snd t)               = case eval e t of
                 VPair _ v -> v
                 _         -> error "Error de tipo en run-time, verificar type checker"



-----------------------
--- quoting
-----------------------

quote :: Value -> Term
quote (VLam t f) = Lam t f

----------------------
--- type checker
-----------------------

-- type checker
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=) :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 = err $ "se esperaba " ++
                         render (printType t1) ++
                         ", pero " ++
                         render (printType t2) ++
                         " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notPairError :: Type -> Either String Type
notPairError t1 = err $ "se esperaba un tipo Pair, pero " ++
                       render (printType t1) ++
                       " fue inferido."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free n) = case lookup n e of
                        Nothing -> notfoundError n
                        Just (_,t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt ->
                       infer' c e u >>= \tu ->
                       case tt of
                         Fun t1 t2 -> if (tu == t1)
                                        then ret t2
                                        else matchError t1 tu
                         _         -> notfunError tt
infer' c e (Lam t u) = infer' (t:c) e u >>= \tu ->
                       ret $ Fun t tu
infer' c e (Let t1 t2) = infer' c e t1 >>= \tt ->
                         infer' (tt:c) e t2
infer' c e (As u t) = infer' c e u >>= \tt ->
                      if t == tt then ret t
                      else matchError t tt 
infer' _ _ (TUnit) = ret Unit
infer' c e (TPair t u) = infer' c e t >>= \tt ->
                         infer' c e u >>= \tu ->
                         Pair tt tu
infer' c e (Fst t)    = infer' c e t >>= \tt ->
                        case tt of
                          Pair tu _ -> tu
                          _         -> notPairError tt
infer' c e (Snd t)    = infer' c e t >>= \tt ->
                        case tt of
                          Pair _ tu -> tu
                          _         -> notPairError tt

----------------------------------
