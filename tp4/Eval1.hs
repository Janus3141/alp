module Eval1 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)  

-- Estados
type Env = [(Variable,Integer)]

-- Estado nulo
initState :: Env
initState = []

-- Mónada estado
newtype State a = State { runState :: Env -> (a, Env) }

instance Monad State where
    return x = State (\s -> (x, s))
    m >>= f = State (\s -> let (v, s') = runState m s in
                           runState (f v) s')

-- Para calmar al GHC
instance Functor State where
    fmap = liftM
 
instance Applicative State where
    pure   = return
    (<*>)  = ap      


-- Clase para representar mónadas con estado de variables
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Variable -> m Integer
    -- Cambia el valor de una variable
    update :: Variable -> Integer -> m ()


instance MonadState State where
    lookfor v = State (\s -> (lookfor' v s, s))
                where lookfor' v ((u, j):ss) | v == u = j
                                             | v /= u = lookfor' v ss
    update v i = State (\s -> ((), update' v i s))
                 where update' v i [] = [(v, i)]
                       update' v i ((u, _):ss) | v == u = (v, i):ss
                       update' v i ((u, j):ss) | v /= u = (u, j):(update' v i ss)



-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (evalComm p) initState)



-- Evalua un comando en un estado dado
evalComm :: MonadState m => Comm -> m ()
evalComm c = case c of
    Skip         -> return ()
    Let v e      -> evalIntExp e >>= \x -> update v x
    Seq c1 c2    -> evalComm c1 >> evalComm c2 >> return ()
    Cond b c1 c2 -> evalBoolExp b >>= \x -> if x 
                                            then evalComm c1
                                            else evalComm c2
    Repeat c' b  -> do evalComm c'
                       b' <- evalBoolExp b
                       if b' then return ()
                             else evalComm (Repeat c' b)



-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: MonadState m => IntExp -> m Integer
evalIntExp e = case e of
    Const i   -> return i
    Var v     -> lookfor v
    UMinus a  -> let a' = evalIntExp a
                 in a' >>= \x -> return (-x)
    Plus a b  -> evalHelper a b (+)
    Minus a b -> evalHelper a b (-)
    Times a b -> evalHelper a b (*)
    Div a b   -> evalHelper a b (div)
   where evalHelper a b f = do x <- evalIntExp a
                               y <- evalIntExp b
                               return $ f x y


-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: MonadState m => BoolExp -> m Bool
evalBoolExp e = case e of
    BTrue   -> return True
    BFalse  -> return False
    Eq a b  -> evalIntHelper a b (==)
    Lt a b  -> evalIntHelper a b (<)
    Gt a b  -> evalIntHelper a b (>)
    And a b -> evalBoolHelper a b (&&)
    Or a b  -> evalBoolHelper a b (||)
    Not a   -> evalBoolExp a >>= \x -> return $ not x
  where evalIntHelper a b f = do x <- evalIntExp a
                                 y <- evalIntExp b
                                 return $ f x y
        evalBoolHelper a b f = do x <- evalBoolExp a
                                  y <- evalBoolExp b
                                  return $ f x y


