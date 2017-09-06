module Eval3 (eval) where

import AST

-- Estados
type State = [(Variable,Integer)]

-- Errores
data Err = DivByZero | UndefVar

-- Trazas
type Trace = String

-- Estado nulo
initState :: State
initState = []


-- Busca el valor de una variabl en un estado
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
eval :: Comm -> (Either Err State,Trace)
eval p = evalComm p initState


-- Evalua un comando en un estado dado
evalComm :: Comm -> State -> (Either Err State, Trace)
evalComm com st = case com of
                   Skip        -> (Right st, "-> Skip ")
                   Let v exp   -> case evalIntExp exp st of
                                    (Right int,t) -> (Right (update v int st),
                                                      t++"-> Let "++v++" "++(show int)++" ")
                                    (Left err,t) -> (Left err,t)
                   Seq c c'    -> let (st',t) = evalComm c st
                                  in case st' of
                                        Right st'' -> let (res,t') = evalComm c' st''
                                                      in (res,t++"-> Seq "++t')
                                        Left err   -> (Left err,t)
                   Cond b c c' -> case evalBoolExp b st of
                                    (Right b',t) -> if b' then (res,t++"-> Cond True "++t')
                                                    else (res',t++"-> Cond False "++t'')
                                                    where (res,t') = evalComm c st
                                                          (res',t'') = evalComm c' st
                                    (Left err,t) -> (Left err,t)
                   Repeat c b  -> case evalComm c st of
                                    (Right st',t) -> case evalBoolExp b st' of
                                                      (Right b',t') -> if b' then (Right st',t++t'++"-> EndRepeat ")
                                                                       else let (res,t'') = evalComm (Repeat c b) st'
                                                                            in (res,t++t'++"-> Repeat "++t'')
                                                      (Left err,t') -> (Left err,t++t')
                                    (Left err,t)  -> (Left err,t)


-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: IntExp -> State -> (Either Err Integer, Trace)
evalIntExp exp st = case exp of
                     Const n       -> (Right n, "-> Const "++show n++" ")
                     Var v         -> (lookfor v st, "-> Var "++v++" ")
                     UMinus e      -> case evalIntExp e st of
                                        (Right n,t) -> (Right (-n), t++"-> UMinus "++(show n)++" ")
                                        (Left err,t) -> (Left err,t)
                     Plus e e'     -> appInt "Plus" (+) e e' st
                     Minus e e'    -> appInt "Minus" (-) e e' st
                     Times e e'    -> appInt "Times" (*) e e' st
                     Div e e'      -> let n  = evalIntExp e st
                                          n' = evalIntExp e' st
                                      in case (n,n') of
                                            ((Right v1,t1), (Right 0,t2))  -> (Left DivByZero,t1++t2++
                                                                               "-> Div "++(show v1)++" 0")
                                            ((Right v1,t1), (Right v2,t2)) -> (Right (div v1 v2),t1++t2++
                                                                               "-> Div "++(show v1)++" "++(show v2)++" ")
                                            ((Left err,t),_)               -> (Left err,t)


-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: BoolExp -> State -> (Either Err Bool, Trace) 
evalBoolExp exp st = case exp of
                      BTrue -> (Right True, "-> BTrue ")
                      BFalse -> (Right False, "-> BFalse ")
                      Eq e e' -> appInt "Eq" (==) e e' st 
                      Lt e e' -> appInt "Lt" (<) e e' st 
                      Gt e e' -> appInt "Gt" (>) e e' st 
                      And e e' -> appBool "And" (&&) e e' st
                      Or e e' -> appBool "Or" (||) e e' st
                      Not e -> case evalBoolExp e st of
                                (Right b,t) -> (Right (not b),t++"-> Not "++(show b)++" ")
                                (Left err,t) -> (Left err,t)


-- Funciones auxiliares
app :: (Show b) => (a -> State -> (Either Err b,Trace)) -> String -> (b -> b -> c)
        -> a -> a -> State -> (Either Err c, Trace)
app eval cons f a b st = let e1 = eval a st
                             e2 = eval b st
                         in case (e1,e2) of
                             ((Right v1,t1), (Right v2,t2)) -> (Right (f v1 v2),t1++t2++
                                                                "-> "++cons++" "++(show v1)++" "++(show v2)++" ")
                             ((Left err,t),_)               -> (Left err,t)
                             (_,(Left err,t))               -> (Left err,t)



appInt = app evalIntExp
appBool = app evalBoolExp

