module Eval3 (eval) where

import AST

-- Estados
type State = [(Variable,Integer)]

-- Errores
data Err = DivByZero | UndefVar
            deriving Show

-- Trazas
data Op = Const' | Var' | UMinus' | Plus' | Minus' | Times' | Div'
            | Choice' | BTrue' | BFalse' | Eq' | Lt' | Gt' | And'
            | Or' | Not' | Skip' | Let' | Seq' | Cond' | Repeat'
          deriving Show

type Trace = [(Op,State)]

-- Estado nulo
initState :: State
initState = []

-- Traza nula


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
eval :: Comm -> (Either Err State, Trace)
eval p = evalComm p initState

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> (Either Err State, Trace)
evalComm com st = case com of
                   Skip        -> (Right st,[(Skip',st)])
                   Let v exp   -> case evalIntExp exp st of
                                    (Right int,t) -> let st' = update v int st
                                                     in (Right st',((Let',st'):t))
                                    (Left err,t)  -> (Left err,t)
                   Seq c c'    -> let st' = evalComm c st
                                  in case st' of
                                      (Right st'',t) -> case evalComm c' st'' of
                                                         (Left err,t')        -> (Left err,t'++t)
                                                         (r,[])               -> (r,t)
                                                         (r,t'@((_,sta):ts))  -> (r,((Seq',sta):t'))
                                      (Left err,t)   -> (Left err,t)
                   Cond b c c' -> case evalBoolExp b st of
                                   (Right b',t) -> let c'' = if b' then c else c'
                                                   in case evalComm c'' st of
                                                       (Left err,t')       -> (Left err,t'++t)
                                                       (r,t'@((_,sta):ts)) -> (r,((Cond',sta):t')++t)
                                   (Left err,t) -> (Left err,t)
                   Repeat c b  -> case evalComm c st of
                                   (Right st',t) -> case evalBoolExp b st' of
                                                     (Left err,t')              -> (Left err, t'++t)
                                                     (Right b',t'@((_,sta):ts)) -> if b' then (Right st',((Repeat',sta):t')++t)
                                                                                   else case evalComm (Repeat c b) st' of
                                                                                         (Left err,t'') -> (Left err,t''++t'++t)
                                                                                         (r,t''@((_,sta'):ts')) -> (r,((Repeat',sta'):t'')++t'++t)
                                   (Left err,t) -> (Left err,t)

-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalIntExp :: IntExp -> State -> (Either Err Integer, Trace)
evalIntExp exp st = case exp of
                     Const n       -> (Right n,[(Const',st)])
                     Var v         -> (lookfor v st,[(Var',st)])
                     UMinus e      -> case evalIntExp e st of
                                        (Right n,t) -> (Right (-n),((UMinus',st):t))
                                        (Left err,t) -> (Left err,t)
                     Plus e e'     -> appInt Plus' (+) e e' st
                     Minus e e'    -> appInt Minus' (-) e e' st
                     Times e e'    -> appInt Times' (*) e e' st
                     Div e e'      -> let n  = evalIntExp e st
                                          n' = evalIntExp e' st
                                      in case (n,n') of
                                          ((Right v1,t),(Right 0,t'))  -> (Left DivByZero,((Div',st):t')++t)
                                          ((Right v1,t),(Right v2,t')) -> (Right (div v1 v2),((Div',st):t')++t)
                                          ((Left err,t),_)             -> (Left err,t)
                                          (_,err)                      -> err
                     Choice b e e' -> case evalBoolExp b st of
                                        (Right b',t) -> let e'' = if b' then e else e'
                                                        in let (exp',t') = evalIntExp e'' st
                                                           in (exp',((Choice',st):t')++t)
                                        (Left err,t) -> (Left err,t)

-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalBoolExp :: BoolExp -> State -> (Either Err Bool, Trace)
evalBoolExp exp st = case exp of
                      BTrue -> (Right True, [(BTrue',st)])
                      BFalse -> (Right False, [(BFalse',st)])
                      Eq e e' -> appInt Eq' (==) e e' st 
                      Lt e e' -> appInt Lt' (<) e e' st 
                      Gt e e' -> appInt Gt' (>) e e' st 
                      And e e' -> appBool And' (&&) e e' st
                      Or e e' -> appBool Or' (||) e e' st
                      Not e -> case evalBoolExp e st of
                                (Right b,t)  -> (Right (not b),((Not',st):t))
                                (Left err,t) -> (Left err,t)


-- Funciones auxiliares
app :: (a -> State -> (Either Err b,Trace)) -> Op -> (b -> b -> c) -> a -> a -> State -> (Either Err c,Trace)
app eval op f a b st = let e1 = eval a st
                           e2 = eval b st
                       in case (e1,e2) of
                           ((Right v1,t),(Right v2,t')) -> (Right (f v1 v2),((op,st):t)++t')
                           ((Left err,t),_)         -> (Left err,t)
                           (_,(Left err,t))         -> (Left err,t)

appInt = app evalIntExp
appBool = app evalBoolExp

