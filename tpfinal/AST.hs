   module AST where

import Types
import Graphics.PDF


-- El contenido que puede tener un rectangulo al momento de parsear.
-- El primer tipo String es para las direcciones de las imagenes que
-- se insertan. El segundo es para el texto del texto 'flotante'
-- (el que va separado del cuerpo del documento).
type ParserCont = Cont String String


type Variable = String


-- Tipo para longitudes: pixeles, milimetros y pulgadas.
data Length = Pixel Integer | MM Integer | Inch Integer
              deriving Show

-- Un par de longitudes, utilizado para los tamaños de hoja
-- y para el posicionamiento de rectangulos.
type LenPair = (Exp Length, Exp Length)


type IntPair = (Integer,Integer)


-- Los rectangulos usados al parsear. Se inician con 'rect' (ParserRect),
-- y pueden modificarse con las demas funciones (se devuelve un nuevo
-- rectangulo).
data ParserRect = ParserRect (Exp LenPair, Exp LenPair) (Exp LenPair)
                | Clean (Exp ParserRect)
                | In_frame (Exp ParserRect) String
                | Out_frame (Exp ParserRect) String
                | Rect_set (Exp ParserRect) (Exp ParserCont)
    deriving Show


-- Tipo para expresiones. En algunos casos puede esperarse un valor,
-- una variable, o una operacion (Op nombre_operacion [argumentos])
-- que evalua a un valor del tipo esperado.
data Exp a = Value a
           | Op Variable [Exp VType]
           | Var Variable
    deriving Show


-- Tipos de valor que puede tomar una variable
data VType = VLength  Length
           | VLenPair LenPair
           | VRect    ParserRect
           | VCont    ParserCont
           | VInt     Integer
           | VDoc     Doc


instance Show VType where
    show (VLength _)  = "\"longitud\""
    show (VLenPair _) = "\"longitudes\""
    show (VRect _)    = "\"rectangulo\""
    show (VCont _)    = "\"contenido\""
    show (VInt _)     = "\"entero\""
    show (VDoc _)     = "\"sentencias/texto\""


-- Sentencias, modificadores de texto y el cuerpo del
-- documento (Text_value).
data Stmt = Def Variable [Variable] VType
          | StmtVar Variable
          | StmtOp Variable [Exp VType]
          | PPI Integer
          | Add_rects [Exp ParserRect]
          | Page_dflt (Exp LenPair) [Exp ParserRect]
          | Newpage_dflt
          | Newpage (Exp LenPair) [Exp ParserRect]
          | Text_dflt FontName Integer
          | Text_bold
          | Text_bold_off
          | Text_italics
          | Text_ital_off
          | Text_resize (Exp Integer)
          | Text_size_normal
          | Text_font FontName
          | Text_font_normal
          | Text_return
          | Text_line_space (Exp Integer)
          | Text_value String
          | Include FilePath
          | Debug
    deriving Show


type Doc = [Stmt]


