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
searchFor :: String -> [(String,Int)] -> Int
searchFor _ []           = -1
searchFor var ((v,n):xs) = if var == v then n else searchFor var xs

-- Incremento la distancia de las variables a las ligaduras
incAdd :: Name -> [(Name,Int)] -> [(Name,Int)]
inc []         = []
inc ((v,n):xs) = (v,n+1):(inc xs)


conversion :: LamTerm -> Term
conversion term = conversion' [] term
                  where conversion' vars LVar var     = let n = searchFor var vars
                                                        in if n >= 0
                                                           then (Bound n)
                                                           else (Free var)
                        conversion' vars App lam lam' = ((conversion' vars lam)
                                                        :@:
                                                        (conversion' vars1 lam'))
                        conversion' vars Abs var lam  = let vars' = incAdd var vars
                                                        in conversion' vars' lam


  
-------------------------------
-- Sección 3 - Evaluación
-------------------------------

shift :: Term -> Int -> Int -> Term
shift = undefined
  
  
subst :: Term -> Term -> Int -> Term
subst = undefined	


eval :: NameEnv Term -> Term -> Term
eval = undefined
    
    
    
    
    
