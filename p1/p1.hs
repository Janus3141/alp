import Parsing
import Control.Applicative ((<|>)) 


------ Ejercicio 2 ------

expr :: Parser Int
expr = do t <- term
          e <- expr'
          return (e t)


expr' :: Parser (Int -> Int)
expr' = do char '+'
	   t <- term
	   ts <- expr'
           return (ts.(+t))
          <|> do char '-'
                 t <- term
                 ts <- expr'
                 return (\x -> ts (x-t))
	        <|> return id


term :: Parser Int
term = do f <- factor
          t <- term'
          return (t f)


term' :: Parser (Int -> Int)
term' = do char '*'
	   f <- factor
           fs <- term'
           return (fs.(*f))
          <|> do char '/'
                 f <- factor
                 fs <- term'
                 return (\x -> fs (div x f))
                <|> return id


factor :: Parser Int
factor = do n <- nat
            return n
           <|> do char '('
                  e <- expr
                  char ')'
                  return e


eval' :: String -> Int
eval' xs = fst (head (parse expr xs))


------ Ejercicio 3 ------

paren :: Parser a -> Parser a
paren p  = do char '('
              p' <- p
              char ')'
	      return p'
             <|> p

