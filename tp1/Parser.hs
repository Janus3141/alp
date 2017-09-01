module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import AST

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do 
                  whiteSpace lis
                  t <- p
                  eof
                  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentStart  = "/*"
                                  , commentEnd    = "*/"
                                  , commentLine   = "//"
                                  , opLetter      = char '='
                                  , reservedNames = ["true","false","skip","if",
                                                     "then","else","end", "while","do"]
                                  })
  
----------------------------------
--- Parser de expressiones enteras
-----------------------------------

divmul :: Parser (IntExp -> IntExp -> IntExp)
divmul = do reservedOp lis "*"
            return Times
       <|> do reservedOp lis "/"
              return Div


ressum :: Parser (IntExp -> IntExp -> IntExp)
ressum = do reservedOp lis "+"
            return Plus
       <|> do reservedOp lis "-"
              return Minus


baseExp :: Parser IntExp
baseExp = do n <- natural lis
             return (Const n)
        <|> do n <- identifier lis
               return (Var n)


intexp :: Parser IntExp
intexp = chainl1 term ressum 

term :: Parser IntExp
term = chainl1 factor divmul

factor :: Parser IntExp
factor = parens lis intexp
       <|> baseExp
       <|> do reservedOp lis "-"
              fac <- factor
              return (UMinus fac)


-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

compOp :: Parser (IntExp -> IntExp -> BoolExp)
compOp = do reservedOp lis "="
            return Eq
       <|> do reservedOp lis "<"
              return Lt
       <|> do reservedOp lis ">"
              return Gt


orOp :: Parser (BoolExp -> BoolExp -> BoolExp)
orOp = do reservedOp lis "|"
          return Or


andOp :: Parser (BoolExp -> BoolExp -> BoolExp)
andOp = do reservedOp lis "&"
           return And


baseBool :: Parser BoolExp
baseBool = do reserved lis "true"
              return BTrue
         <|> do reserved lis "false"
                return BFalse
         <|> do exp1 <- intexp
                op <- compOp
                exp2 <- intexp
                return (op exp1 exp2)


boolexp :: Parser BoolExp
boolexp = chainl1 boolexp2 orOp

boolexp2 :: Parser BoolExp
boolexp2 = chainl1 boolexp3 andOp

boolexp3 :: Parser BoolExp
boolexp3 = parens lis boolexp <|> baseBool <|> do reservedOp lis "~"
                                                  boole <- boolexp3
                                                  return (Not boole)

-----------------------------------
--- Parser de comandos
-----------------------------------

seqComm :: Parser (Comm -> Comm -> Comm)
seqComm = do reservedOp lis ";"
             return Seq


comm :: Parser Comm
comm = chainl comm2 seqComm


comm2 :: Parser Comm
comm2 = do var <- identifier lis
          reservedOp lis ":="
          n <- intexp
          return (Let var n)
     <|> do reserved lis "if"
            b <- boolexp
            reserved lis "then"
            c1 <- comm
            reserved lis "else"
            c2 <- comm
            reserved lis "end"
            return (Cond b c1 c2)
     <|> do reserved lis "repeat"
            c <- comm
            reserved lis "until"
            b <- boolexp
            reserved lis "end"
            return (Repeat c b)
     <|> do reserved lis "skip"


------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
