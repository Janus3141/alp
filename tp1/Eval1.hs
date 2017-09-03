module Eval1 (eval) where

import AST

-- Estados
type State = [(Variable,Integer)]

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variabl en un estado
-- Completar la definicion
lookfor :: Variable -> State -> Integer
lookfor v ((name,value):st) = if name == v then value else lookfor v st

-- Cambia el valor de una variable en un estado
-- Completar la definicion
update :: Variable -> Integer -> State -> State
update v value []               = [(v,value)]
update v value ((v',value'):st) = if v' == v 
                                  then ((v,value):st)
                                  else ((v',value'):(update v value st))

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> State
evalComm com st = case com of
                   Skip        -> st
                   Let v exp   -> update v (evalIntExp exp st) st
                   Seq c c'    -> let st' = evalComm c st
                                  in evalComm c' st'
                   Cond b c c' -> if evalBoolExp b st
                                  then evalComm c st
                                  else evalComm c' st
                   Repeat c b  -> let st' = evalComm c st
                                  in if evalBoolExp b st' then st'
                                     else evalComm (Repeat c b) st'

-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalIntExp :: IntExp -> State -> Integer
evalIntExp exp st = case exp of
                     Const n       -> n
                     Var v         -> lookfor v st
                     UMinus e      -> -(evalIntExp e st)
                     Plus e e'     -> app (+) e e'
                     Minus e e'    -> app (-) e e'
                     Times e e'    -> app (*) e e'
                     Div e e'      -> app (div) e e'
                     Choice b e e' -> if evalBoolExp b st
                                      then evalIntExp e st
                                      else evalIntExp e' st
                    where app f a b = let n  = evalIntExp a st
                                          n' = evalIntExp b st
                                      in f n n'

-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalBoolExp :: BoolExp -> State -> Bool
evalBoolExp exp st = case exp of
                      BTrue -> True
                      BFalse -> False
                      Eq e e' -> let exp1 = evalIntExp e st
                                     exp2 = evalIntExp e' st
                                 in exp1 == exp2
                      Lt e e' -> let exp1 = evalIntExp e st
                                     exp2 = evalIntExp e' st
                                 in exp1 < exp2
                      Gt e e' -> let exp1 = evalIntExp e st
                                     exp2 = evalIntExp e' st
                                 in exp1 > exp2
                      And e e' -> let exp1 = evalBoolExp e st
                                      exp2 = evalBoolExp e' st
                                  in exp1 && exp2
                      Or e e' -> let exp1 = evalBoolExp e st
                                     exp2 = evalBoolExp e' st
                                 in exp1 || exp2
                      Not e -> not (evalBoolExp e st)

