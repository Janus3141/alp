module Eval2 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

-- Estados
type Env = [(Variable,Integer)]

-- Estado nulo
initState :: Env
initState = []

--Mensajes de error
data Error = DivByZero | UndefVar Variable

instance Show Error where
  show DivByZero = "Error: División por cero."
  show (UndefVar v) = "Error: Variable " ++ show v ++ " indefinida."


-- Mónada estado
newtype StateError a = StateError { runStateError :: Env -> Either Error (a, Env) }

-- Para calmar al GHC
instance Functor StateError where
    fmap = liftM

instance Applicative StateError where
    pure   = return
    (<*>)  = ap


-- Se devuelve a main para imprimir mejores mensajes
data PrettyPrint = Error Error | Environment Env

instance Show PrettyPrint where
    show (Error err) = show err
    show (Environment env) = "Estado final\n" ++ show' env
                      where show' [] = ""
                            show' ((v,i):xs) = v ++ ": " ++
                                               show i ++ "\n" ++
                                               show' xs


-- Clase para representar mónadas con estado de variables
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Variable -> m Integer
    -- Cambia el valor de una variable
    update :: Variable -> Integer -> m ()


-- Clase para representar mónadas que lanzan errores
class Monad m => MonadError m where
    -- Lanza un error
    throw :: Error -> m a


instance Monad StateError where
    return x = StateError (\s -> Right (x,s))
    m >>= f  = StateError (\s -> case runStateError m s of
                                   Left err     -> Left err
                                   Right (x,s') -> runStateError (f x) s')



instance MonadState StateError where
    lookfor v = StateError (\s -> lookfor' s s)
                where lookfor' [] = \_ -> Left $ UndefVar v
                      lookfor' ((v',n):ss) | v == v' = \x -> Right (n,x)
                                           | v /= v' = lookfor' ss
    update v i = StateError (\s -> Right ((), updt s))
                 where updt [] = [(v,i)]
                       updt ((v',n):xs) | v == v' = (v,i):xs
                                        | v /= v' = (v',n):(updt xs)


instance MonadError StateError where
    throw err = StateError (\_ -> Left err)


-- Evalua un programa en el estado nulo
eval :: Comm -> PrettyPrint
eval p = case runStateError (evalComm p) initState of
            Left err      -> Error err
            Right (v,env) -> Environment env

-- Evalua un comando en un estado dado
evalComm :: (MonadState m, MonadError m) => Comm -> m ()
evalComm c = case c of
    Skip         -> return ()
    Let v e      -> evalIntExp e >>= \x -> update v x
    Seq c1 c2    -> evalComm c1 >> evalComm c2 >> return ()
    Cond b c1 c2 -> do b' <- evalBoolExp b
                       if b' then evalComm c1
                             else evalComm c2
    Repeat c' b  -> do evalComm c'
                       b' <- evalBoolExp b
                       if b' then return ()
                             else evalComm (Repeat c' b)


-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: (MonadState m, MonadError m) => IntExp -> m Integer
evalIntExp e = case e of
    Const i   -> return i
    Var v     -> lookfor v
    UMinus a  -> evalIntExp a >>= \x -> return (-x)
    Plus a b  -> helper a b (+)
    Minus a b -> helper a b (-)
    Times a b -> helper a b (*)
    Div a b   -> do b' <- evalIntExp b
                    if b' == 0
                     then throw DivByZero
                     else evalIntExp a >>= \a' -> return $ div a' b'
    where helper a b f = do a' <- evalIntExp a
                            b' <- evalIntExp b
                            return $ f a' b'


-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: (MonadState m, MonadError m) => BoolExp -> m Bool
evalBoolExp e = case e of
    BTrue   -> return True
    BFalse  -> return False
    Eq a b  -> helper a b (==)
    Lt a b  -> helper a b (<)
    Gt a b  -> helper a b (>)
    And a b -> helper2 a b (&&)
    Or a b  -> helper2 a b (||)
    Not a   -> evalBoolExp a >>= \x -> return $ not x
    where helper a b f = do a' <- evalIntExp a
                            b' <- evalIntExp b
                            return $ f a' b'
          helper2 a b f = do a' <- evalBoolExp a
                             b' <- evalBoolExp b
                             return $ f a' b'
