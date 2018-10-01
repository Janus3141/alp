   module AST where

import Types
import Graphics.PDF


type ParserCont = Cont String String


type Variable = String


data Length = Pixel Integer | CM Integer | Inch Integer
              deriving Show


type LenPair = (Exp Length, Exp Length)


type IntPair = (Integer,Integer)


data ParserRect = ParserRect (Exp LenPair, Exp LenPair) (Exp LenPair)
                | Clean (Exp ParserRect)
                | In_frame (Exp ParserRect) String
                | Out_frame (Exp ParserRect) String
                | Rect_set (Exp ParserRect) (Exp ParserCont)
    deriving Show


data Exp a = Value a
           | Op Variable [Exp VType]
           | Var Variable
    deriving Show


data VType = VLength  Length
           | VLenPair LenPair
           | VRect    ParserRect
           | VCont    ParserCont
           | VInt     Integer
           | VDoc     Doc
    deriving Show


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
          | Include String
          | Debug
    deriving Show


type Doc = [Stmt]


