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
conversion' b (LtFst t)      = Fst (conversion' b t)
conversion' b (LtSnd t)      = Snd (conversion' b t)
conversion' b (LtPair t u)   = TPair (conversion' b t) (conversion' b u)
conversion' _ (LtZero)       = Zero
conversion' b (LtSucc t)     = Succ (conversion' b t)
conversion' b (LtR t1 t2 t3) = let t1' = conversion' b t1
                                   t2' = conversion' b t2
                                   t3' = conversion' b t3
                               in R t1' t2' t3'


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
sub i t (Fst u)               = Fst (sub i t u)
sub i t (Snd u)               = Snd (sub i t u)
sub i t (TPair u v)           = TPair (sub i t u) (sub i t v)
sub _ _ (Zero)                = Zero
sub i t (Succ u)              = Succ (sub i t u)
sub i t (R u1 u2 u3)          = let u1' = sub i t u1
                                    u2' = sub i t u2
                                    u3' = sub i t u3
                                in R u1' u2' u3'


-- evaluador de términos
eval :: NameEnv Value Type -> Term -> Value
eval _ (Bound _)             = error "variable ligada inesperada en eval"
eval e (Free n)              = fst $ fromJust $ lookup n e
eval _ (Lam t u)             = VLam t u
eval e (Lam _ u :@: v)       = let v' = eval e v
                               in eval e (sub 0 (quote v') u)
eval e (u :@: v)             = case eval e u of
                 VLam t u' -> eval e (Lam t u' :@: v)
                 _         -> error "Error de tipo en run-time, verificar type checker"
eval e (Let u u')            = let v = eval e u
                               in eval e (sub 0 (quote v) u')
eval e (As u _)              = eval e u
eval _ (TUnit)               = VUnit
eval e (TPair u v)           = let u' = eval e u
                               in VPair u' (eval e v)
eval e (Fst t)               = case eval e t of
                 VPair v _ -> v
                 _         -> error "Error de tipo en run-time, verificar type checker"
eval e (Snd t)               = case eval e t of
                 VPair _ v -> v
                 _         -> error "Error de tipo en run-time, verificar type checker"
eval _ (Zero)                = VN VZero
eval e (Succ n)              = let VN n' = eval e n
                               in VN $ VSucc n'
eval e (R t1 t2 t3)          = case eval e t3 of
                 VN VZero     -> eval e t1
                 VN (VSucc n) -> let n' = quote' n
                                 in eval e $ (t2 :@: (R t1 t2 $ n')) :@: n'
                 _            -> error "Error de tipo en run-time, verificar type checker"



-----------------------
--- quoting
-----------------------

quote :: Value -> Term
quote (VLam t f)   = Lam t f
quote VUnit        = TUnit
quote (VPair v v') = TPair (quote v) (quote v')
quote (VN vn)      = quote' vn

quote' :: ValNum -> Term
quote' VZero     = Zero
quote' (VSucc n) = Succ $ quote' n

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
infer' c e (Lam t u)   = infer' (t:c) e u >>= \tu ->
                         ret $ Fun t tu
infer' c e (Let t1 t2) = infer' c e t1 >>= \tt ->
                         infer' (tt:c) e t2
infer' c e (As u t)    = infer' c e u >>= \tt ->
                         if t == tt then ret t
                         else matchError t tt 
infer' _ _ TUnit       = ret Unit
infer' c e (TPair t u) = infer' c e t >>= \tt ->
                         infer' c e u >>= \tu ->
                         ret $ Pair tt tu
infer' c e (Fst t)    = infer' c e t >>= \tt ->
                        case tt of
                          Pair tu _ -> ret tu
                          _         -> notPairError tt
infer' c e (Snd t)    = infer' c e t >>= \tt ->
                        case tt of
                          Pair _ tu -> ret tu
                          _         -> notPairError tt
infer' c e Zero       = ret Nat
infer' c e (Succ n)   = infer' c e n >>= \tt ->
                        case tt of
                          Nat -> ret Nat
                          _   -> matchError Nat tt
infer' c e (R t f n)  = infer' c e t >>= \tt ->
                        infer' c e f >>= \tu ->
                        if tu == Fun tt (Fun Nat tt) then
                            case infer' c e n of
                              Right Nat -> ret tt
                              Right x   -> matchError Nat x
                              Left x    -> err x
                        else matchError (Fun tt (Fun Nat tt)) tu

----------------------------------
