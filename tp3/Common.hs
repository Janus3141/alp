module Common where

  -- Comandos interactivos o de archivos
  data Stmt i = Def String i           --  Declarar un nuevo identificador x, let x = t
              | Eval i                 --  Evaluar el término
    deriving (Show)
  
  instance Functor Stmt where
    fmap f (Def s i) = Def s (f i)
    fmap f (Eval i)  = Eval (f i)

  -- Tipos de los nombres
  data Name
     =  Global  String
    deriving (Show, Eq)

  -- Entornos
  type NameEnv v t = [(Name, (v, t))]

  -- Tipo de los tipos
  data Type = Base 
            | Unit
            | Fun Type Type
            | Pair Type Type
            | Nat
            deriving (Show, Eq)
  
  -- Términos con nombres
  data LamTerm  =  LVar String
                |  Abs String Type LamTerm
                |  App LamTerm LamTerm
                |  LtLet String LamTerm LamTerm
                |  LtAs LamTerm Type
                |  LtUnit
                |  LtFst LamTerm
                |  LtSnd LamTerm
                |  LtPair LamTerm LamTerm
                |  LtZero
                |  LtSucc LamTerm
                |  LtR LamTerm LamTerm LamTerm
                deriving (Show, Eq)


  -- Términos localmente sin nombres
  data Term  = Bound Int
             | Free Name 
             | Term :@: Term
             | Lam Type Term
             | Let Term Term
             | As Term Type
             | TUnit
             | TPair Term Term
             | Fst Term
             | Snd Term
             | Zero
             | Succ Term
             | R Term Term Term
          deriving (Show, Eq)

  -- Valores
  data Value = VLam Type Term 
             | VUnit 
             | VPair Value Value
             | VN ValNum

  data ValNum = VZero | VSucc ValNum

  -- Contextos del tipado
  type Context = [Type]

