module Untyped where

import Control.Monad
import Data.List

import Common


----------------------------------------------
-- Seccón 2 - Representación de Términos Lambda 
-- Ejercicio 2: Conversión de Términos
----------------------------------------------
           

-- Busco la variable en la lista de variables ligadas, y devuelvo la distancia
-- a su ligadura si esta ligada, -1 caso contrario.
searchFor :: Name -> [(Name,Int)] -> Int
searchFor _ []           = -1
searchFor var ((v,n):xs) = if var == v then n else searchFor var xs

-- Incremento la distancia de las variables a las ligaduras
incAdd :: Name -> [(Name,Int)] -> [(Name,Int)]
incAdd var []         = [(var,0)]
incAdd var ((v,n):xs) = (v,n+1):(incAdd var xs)


conversion :: LamTerm -> Term
conversion term = conv [] term
                  where conv vars (LVar var)     = let n = searchFor var vars
                                                   in if n >= 0
                                                      then (Bound n)
                                                      else (Free var)
                        conv vars (App lam lam') = ((conv vars lam)
                                                   :@:
                                                   (conv vars lam'))
                        conv vars (Abs var lam)  = let vars' = incAdd var vars
                                                   in (Lam (conv vars' lam))


  
-------------------------------
-- Sección 3 - Evaluación
-------------------------------

shift :: Term -> Int -> Term
shift term d = shift' term d 0
               where shift' (Bound n)  d' c = if n < c 
                                              then (Bound n)
                                              else (Bound (n+d'))
                     shift' (Free v)   d' c = (Free v)
                     shift' (l :@: l') d' c = ((shift' l d' c)
                                              :@:
                                              (shift' l' d' c))
                     shift' (Lam l)    d' c = Lam (shift' l d' (c+1))
  
  
  
subst :: Term -> Term -> Int -> Term
subst (Free var)  t i = (Free var)
subst (Bound n)   t i = if n == i then t else (Bound n)
subst (t1 :@: t2) t i = (subst t1 t i) :@: (subst t2 t i)
subst (Lam t)    t' i = Lam (subst t (shift t' 1) (i+1))



eval :: NameEnv Term -> Term -> Term
eval env (Free var)  = case env of
                         ((n,v):nvs) -> if n == var then v
                                        else eval nvs (Free var)
                         []          -> (Free var)
eval env (Bound n)   = (Bound n)
eval env (t1 :@: t2) = let t1' = eval env t1
                       in case t1' of
                            (Lam t) -> eval env (shift (subst t (shift t2 1) 0) (-1))
                            _       -> (t1' :@: (eval env t2))
eval env (Lam t)     = Lam (eval env t)
