{
module Parse where
import Common
import Data.Maybe
import Data.Char

}

%monad { P } { thenP } { returnP }
%name parseStmt Def
%name parseStmts Defs
%name term Exp

%tokentype { Token }
%lexer {lexer} {TEOF}

%token
    '='     { TEquals }
    ':'     { TColon }
    '\\'    { TAbs }
    '.'     { TDot }
    '('     { TOpen }
    ')'     { TClose }
    '->'    { TArrow }
    VAR     { TVar $$ }
    TYPE    { TType }
    DEF     { TDef }
    LET     { TokLet }
    IN      { TIn }
    AS      { TAs }
    UNIT    { TokUnit }
    TUNIT   { TokTUnit }
    ','     { TComma }
    FST     { TokFst }
    SND     { TokSnd }
    

%right VAR
%left '=' 
%right '->'
%right '\\' '.' LET IN
%left AS 
%right REC
%right SUC 
%right SND FST


%%

Def     :  Defexp                      { $1 }
        |  Exp	                       { Eval $1 }
Defexp  : DEF VAR '=' Exp              { Def $2 $4 } 

Exp     :: { LamTerm }
        : '\\' VAR ':' Type '.' Exp    { Abs $2 $4 $6 }
        | LET VAR '=' Exp IN Exp       { LtLet $2 $4 $6 } --Ejercicio3
        | Exp AS Type                  { LtAs $1 $3 } --Ejercicio4
        | FST Exp                      { LtFst $2 } --Ejercicio8
        | SND Exp                      { LtSnd $2 } --Ejercicio8
        | NAbs                         { $1 }
        
NAbs    :: { LamTerm }
        : NAbs Atom                    { App $1 $2 }
        | Atom                         { $1 }

Atom    :: { LamTerm }
        : UNIT                         { LtUnit } --Ejercicio6
        | VAR                          { LVar $1 }
        | '(' Exp ',' Exp ')'          { LtPair $2 $4 } --Ejercicio8
        | '(' Exp ')'                  { $2 }

Type    : TYPE                         { Base }
        | TUNIT                        { Unit }
        | Type '->' Type               { Fun $1 $3 }
        | '(' Type ',' Type ')'        { Pair $2 $4 }
        | '(' Type ')'                 { $2 }

Defs    : Defexp Defs                  { $1 : $2 }
        |                              { [] }
     
{

data ParseResult a = Ok a | Failed String
                     deriving Show                     
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                         Ok a     -> k a s l
                         Failed e -> Failed e
                         
returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l

happyError :: P a
happyError = \ s i -> Failed $ "Línea "++(show (i::LineNumber))++": Error de parseo\n"++(s)

data Token = TVar String
               | TType
               | TDef
               | TAbs
               | TDot
               | TOpen
               | TClose 
               | TColon
               | TArrow
               | TEquals
               | TEOF
               | TokLet
               | TIn
               | TAs
               | TokUnit
               | TokTUnit
               | TComma
               | TokFst
               | TokSnd
               deriving Show

----------------------------------
lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s)  ->  \line -> lexer cont s (line + 1)
                    (c:cs)
                          | isSpace c -> lexer cont cs
                          | isAlpha c -> lexVar (c:cs)
                    ('-':('-':cs)) -> lexer cont $ dropWhile ((/=) '\n') cs
                    ('{':('-':cs)) -> consumirBK 0 0 cont cs	
	            ('-':('}':cs)) -> \ line -> Failed $ "Línea "++(show line)++": Comentario no abierto"
                    ('-':('>':cs)) -> cont TArrow cs
                    ('\\':cs)-> cont TAbs cs
                    ('.':cs) -> cont TDot cs
                    ('(':cs) -> cont TOpen cs
                    ('-':('>':cs)) -> cont TArrow cs
                    (')':cs) -> cont TClose cs
                    (':':cs) -> cont TColon cs
                    ('=':cs) -> cont TEquals cs
                    (',':cs) -> cont TComma cs --Ejercicio8
                    unknown -> \line -> Failed $ "Línea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                    where lexVar cs = case span isAlpha cs of
                                           ("B",rest)    -> cont TType rest
                                           ("def",rest)  -> cont TDef rest
                                           ("let",rest)  -> cont TokLet rest --Ejercicio3
                                           ("in",rest)   -> cont TIn rest --Ejercicio3
                                           ("as",rest)   -> cont TAs rest --Ejercicio4
                                           ("unit",rest) -> cont TokUnit rest --Ejercicio6
                                           ("Unit",rest) -> cont TokTUnit rest
                                           ("fst",rest)  -> cont TokFst rest --Ejercicio8
                                           ("snd",rest)  -> cont TokSnd rest --Ejercicio8
                                           (var,rest)    -> cont (TVar var) rest
                          consumirBK anidado cl cont s = case s of
                                                                      ('-':('-':cs)) -> consumirBK anidado cl cont $ dropWhile ((/=) '\n') cs
		                                                      ('{':('-':cs)) -> consumirBK (anidado+1) cl cont cs	
		                                                      ('-':('}':cs)) -> case anidado of
			                                                                     0 -> \line -> lexer cont cs (line+cl)
			                                                                     _ -> consumirBK (anidado-1) cl cont cs
		                                                      ('\n':cs) -> consumirBK anidado (cl+1) cont cs
		                                                      (_:cs) -> consumirBK anidado cl cont cs     
                                           
stmts_parse s = parseStmts s 1
stmt_parse s = parseStmt s 1
term_parse s = term s 1
}
