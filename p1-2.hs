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

type H = Either Int Char
type HL = [H] 


helem :: Parser H
helem = 

hlist :: Parser HL
hlist = do char '['
           (do i <- int
               xs <- hlist
               return ((Left i):xs)
              <|> do c <- char
                     xs <- hlist
                     return ((Right c):xs))
          <|> return []
                     
