import Parsing
import Control.Applicative ((<|>))


------ Ejercicio 4 ------

{-
data Expr = Num Int | BinOp Op Expr Expr | Null
data Op = Add | Mul | Min | Div

expr :: Parser Expr
expr = do t <- term
          e <- expr'
          case e of
             Null -> Num t
             binop -> BinOp 


expr' :: Parser Expr
expr' = do char '+'
           t <- term
           ts <- expr'
           return (BinOp Add t ts)
          <|> do char '-'
                 t <- term
                 ts <- expr'
                 return (BinOp Min t ts)
                <|> return 
-}

------ Ejercicio 5 ------

type Heter = Either Int Char
type HList = [Heter] 


helem :: Parser Heter
helem = do i <- int
           return (Left i)
          <|> do c <- letter
                 return (Right c)

helem' :: Parser Heter
helem' = do char ','
            h <- helem
            return h


hlist :: Parser HList
hlist = do char '['
           c <- helem
           cs <- many helem'
           char ']'
           return (c:cs)



------ Ejercicio 6 ------
{-
data Basetype = DInt | DChar | DFloat deriving Show
type Hasktype = [Basetype]


bt :: Parser Basetype
bt = do string "Int"
        return DInt
       <|> do string "Char"
              return DChar
          <|> do string "Float"
                 return DFloat


ht' :: Parser Basetype
ht' = do string " -> "
         b <- bt
         return b


ht :: Parser Hasktype
ht = do b <- bt
        bs <- many ht'
        return (b:bs)
-}


------ Ejercicio 7 ------

data Hasktype = DInt | DChar | Fun Hasktype Hasktype
                deriving Show


bt :: Parser Hasktype
bt = do string "Int"
        return DInt
       <|> do string "Char"
              return DChar


ht :: Parser Hasktype
ht = do b <- bt
        string " -> "
        bs <- ht
        return (Fun b bs)
       <|> do b <- bt
              return b



------ Ejercicio 9 ------
{-
float :: Parser Float
float = do i <- int

constant_expression :: Parser 


type_specifier :: Parser Char
type_specifier = do (string "int"
                   <|> string "char"
                   <|> string "float")
-}

