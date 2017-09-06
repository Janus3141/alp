module Eval1 (eval) where

import AST

-- Estados
type State = [(Variable,Integer)]

-- Estado nulo
initState :: State
initState = []


-- Busca el valor de una variabl en un estado
lookfor :: Variable -> State -> Integer
lookfor v ((name,value):st) = if name == v then value else lookfor v st


-- Cambia el valor de una variable en un estado
update :: Variable -> Integer -> State -> State
update v value []               = [(v,value)]
update v value ((v',value'):st) = if v' == v 
                                  then ((v,value):st)
                                  else ((v',value'):(update v value st))


-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = evalComm p initState


-- Evalua un comando en un estado dado
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
evalIntExp :: IntExp -> State -> Integer
evalIntExp exp st = case exp of
                     Const n       -> n
                     Var v         -> lookfor v st
                     UMinus e      -> -(evalIntExp e st)
                     Plus e e'     -> appInt (+) e e' st
                     Minus e e'    -> appInt (-) e e' st
                     Times e e'    -> appInt (*) e e' st
                     Div e e'      -> appInt (div) e e' st


-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: BoolExp -> State -> Bool
evalBoolExp exp st = case exp of
                      BTrue -> True
                      BFalse -> False
                      Eq e e' -> appInt (==) e e' st
                      Lt e e' -> appInt (<) e e' st
                      Gt e e' -> appInt (>) e e' st
                      And e e' -> appBool (&&) e e' st
                      Or e e' -> appBool (||) e e' st
                      Not e -> not (evalBoolExp e st)


app :: (a -> State -> b) -> (b -> b -> c) -> a -> a -> State -> c
app eval f a b st = let exp1 = eval a st
                        exp2 = eval b st
                    in f exp1 exp2

appInt = app evalIntExp
appBool = app evalBoolExp

