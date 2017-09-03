module Eval2 (eval) where

import AST

-- Estados
type State = [(Variable,Integer)]

-- Errores
data Err = DivByZero | UndefVar

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variabl en un estado
-- Completar la definicion
lookfor :: Variable -> State -> Either Err Integer
lookfor v []                = Left UndefVar
lookfor v ((name,value):st) = if name == v then Right value else lookfor v st

-- Cambia el valor de una variable en un estado
-- Completar la definicion
update :: Variable -> Integer -> State -> State
update v value []               = [(v,value)]
update v value ((v',value'):st) = if v' == v 
                                  then ((v,value):st)
                                  else ((v',value'):(update v value st))

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Err State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> Either Err State
evalComm com st = case com of
                   Skip        -> Right st
                   Let v exp   -> case evalIntExp exp st of
                                    Right int -> Right (update v int st)
                                    Left err -> Left err
                   Seq c c'    -> let st' = evalComm c st
                                  in case st' of
                                        Right st'' -> evalComm c' st''
                                        Left err    -> Left err
                   Cond b c c' -> case evalBoolExp b st of
                                    Right b' -> if b' then evalComm c st
                                                else evalComm c' st
                                    Left err -> Left err
                   Repeat c b  -> case evalComm c st of
                                    Right st' -> case evalBoolExp b st' of
                                                    Right b' -> if b' then Right st'
                                                                else evalComm (Repeat c b) st'
                                                    Left err -> Left err
                                    Left err -> Left err

-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalIntExp :: IntExp -> State -> Either Err Integer
evalIntExp exp st = case exp of
                     Const n       -> Right n
                     Var v         -> lookfor v st
                     UMinus e      -> case evalIntExp e st of
                                        Right n -> Right (-n)
                                        Left err -> Left err
                     Plus e e'     -> appInt (+) e e' st
                     Minus e e'    -> appInt (-) e e' st
                     Times e e'    -> appInt (*) e e' st
                     Div e e'      -> let n  = evalIntExp e st
                                          n' = evalIntExp e' st
                                      in case (n,n') of
                                            (Right v1, Right 0)  -> Left DivByZero
                                            (Right v1, Right v2) -> Right (div v1 v2)
                                            (Left err,_)         -> Left err
                                            (_,err)              -> err
                     Choice b e e' -> case evalBoolExp b st of
                                        Right b' -> if b' then evalIntExp e st
                                                    else evalIntExp e' st
                                        Left err -> Left err

-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalBoolExp :: BoolExp -> State -> Either Err Bool
evalBoolExp exp st = case exp of
                      BTrue -> Right True
                      BFalse -> Right False
                      Eq e e' -> appInt (==) e e' st 
                      Lt e e' -> appInt (<) e e' st 
                      Gt e e' -> appInt (>) e e' st 
                      And e e' -> appBool (&&) e e' st
                      Or e e' -> appBool (||) e e' st
                      Not e -> case evalBoolExp e st of
                                Right b -> Right (not b)
                                Left err -> Left err


-- Funciones auxiliares
app :: (a -> State -> Either Err b) -> (b -> b -> c) -> a -> a -> State -> Either Err c
app eval f a b st = let e1 = eval a st
                        e2 = eval b st
                    in case (e1,e2) of
                         (Right v1, Right v2) -> Right (f v1 v2)
                         (Left err,_)         -> Left err
                         (_,Left err)         -> Left err

appInt = app evalIntExp
appBool = app evalBoolExp

