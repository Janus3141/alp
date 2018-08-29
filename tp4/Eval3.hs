module Eval3 (eval,topp) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

-- Para calmar al GHC
instance Functor StateErrorLog where
    fmap = liftM

instance Applicative StateErrorLog where
    pure   = return
    (<*>)  = ap

-- Estados
type Env = [(Variable,Integer)]

-- Estado nulo
initState :: Env
initState = []

--Mensajes de error
data Error = DivByZero | UndefVar Variable
             deriving Show


-- Trazas de ejecucion
data CommLog = SkipLog
             | LetLog Variable Integer
             | SeqLog
             | CondLog Bool
             | RepeatLog String

type Trace = [CommLog]


-- Monada estado | error | traza
newtype StateErrorLog a = SEL { runSEL :: Env -> (Either Error (a,Env), Trace) }


class Monad m => MonadState m where
    lookfor :: Variable -> m Integer
    update :: Variable -> Integer -> m ()


class Monad m => MonadError m where
    throw :: Error -> m a


class Monad m => MonadLog m where
    save :: CommLog -> m ()


instance Monad StateErrorLog where
    return x = SEL (\s -> (Right (x,s), []))
    m >>= f = SEL (\s -> case runSEL m s of
                            (Left err, t)     -> (Left err, t)
                            (Right (v,s'), t) -> let (v',t') = runSEL (f v) s'
                                                 in (v',t++t'))


instance MonadState StateErrorLog where
    lookfor v = SEL (\s -> (lookfor' s s, []))
                where lookfor' [] = \_ -> Left $ UndefVar v
                      lookfor' ((v',n):ss) | v == v' = \x -> Right (n,x)
                                           | v /= v' = lookfor' ss
    update v i = SEL (\s -> (Right ((), updt s), []))
                 where updt [] = [(v,i)]
                       updt ((v',n):xs) | v == v' = (v,i):xs
                                        | v /= v' = (v',n):(updt xs)


instance MonadError StateErrorLog where
    throw err = SEL (\s -> (Left err, []))


instance MonadLog StateErrorLog where
    save c = SEL (\s -> (Right ((),s), [c]))



-- Evalua un programa en el estado nulo
eval :: Comm -> (Either Error Env, Trace)
eval p = case runSEL (evalComm p) initState of
            (Left err, t)      -> (Left err,t)
            (Right (v,env), t) -> (Right env,t)



-- Evalua un comando en un estado dado
evalComm :: (MonadState m, MonadError m, MonadLog m) => Comm -> m ()
evalComm c = case c of
    Skip         -> save SkipLog
    Let v e      -> evalIntExp e >>= \x -> update v x >> save (LetLog v x)
    Seq c1 c2    -> evalComm c1 >> save SeqLog >> evalComm c2
    Cond b c1 c2 -> do b' <- evalBoolExp b
                       save $ CondLog b'
                       if b' then evalComm c1
                             else evalComm c2
    Repeat c' b  -> do save $ RepeatLog "Repeat"
                       evalComm c'
                       b' <- evalBoolExp b
                       if b' then save $ RepeatLog "End repeat"
                             else evalComm (Repeat c' b)



-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: (MonadState m, MonadError m, MonadLog m) => IntExp -> m Integer
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
evalBoolExp :: (MonadState m, MonadError m, MonadLog m) => BoolExp -> m Bool
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




-- PrettyPrinter
data PrettyPrint = Error Error Trace | Environment Env Trace


showTrace :: Trace -> String
showTrace [] = ""
showTrace (x:xs) = showLog ++ showTrace xs
    where showLog = case x of
                     SkipLog -> "Skip\n"
                     LetLog v n  -> v ++" = "++ show n ++"\n"
                     RepeatLog s -> s ++ "\n"
                     CondLog b   -> "Condition "++ show b ++ "\n"
                     SeqLog      -> ""


instance Show PrettyPrint where
    show (Error err t) = "Error: " ++ showErr err ++ "Traza de ejecucion:\n" ++ showTrace t
         where showErr DivByZero    = "division por cero.\n\n"
               showErr (UndefVar v) = "variable "++ show v ++" no definida.\n"
    show (Environment e t) = "Estado final:\n" ++ showEnv e ++ "Traza de ejecucion:\n" ++ showTrace t
         where showEnv [] = "\n"
               showEnv ((v,n):xs) = v++ ": "++ show n ++ "\n" ++ showEnv xs


topp :: (Either Error Env, Trace) -> PrettyPrint
topp (Left e,t)  = Error e t
topp (Right e,t) = Environment e t


