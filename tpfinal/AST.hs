module AST where

import Types
import Graphics.PDF.Text


type Variable = String


data Length = Pixel Integer | CM Integer | Inch Integer
              deriving Show


type Dimension = (Exp Length, Exp Length)


data GridTerm = Grid_new (Exp GridTerm) (Exp Dimension) Integer Integer
              | Grid_sub (Exp GridTerm) Integer Integer
              | Grid_main Integer
              | Grid_Copy (Exp GridTerm)
              | Grid_none
    deriving Show


-- El tipo se parametriza para reutilizarlo al abrir el archivo de una imagen.
-- El constructor de imagen llevara String al parsear (la direccion del archivo)
-- y llevara JpegFile al abrir la imagen.
data Cont a = Cont_body Alignment
            | Cont_float_txt Alignment String
            | Cont_image a
            | Cont_empty
    deriving Show


data Exp a = Value a
           | Var Variable
    deriving Show


data VType = VLength Length
           | VDim    Dimension
           | VGrid   GridTerm
           | VCont   (Cont String)
           | VInt    Integer
           | VDoc    Doc
    deriving Show


data Stmt = Def Variable [Variable] VType
          | StmtVar Variable
          | Op Variable [Exp VType]
          | PPI Integer
          | Vert (Exp GridTerm) Integer (Exp Length)
          | Horz (Exp GridTerm) Integer (Exp Length)
          | Clean (Exp GridTerm)
          | In_frame (Exp GridTerm) String
          | Out_frame (Exp GridTerm) String
          | Set_cont (Exp GridTerm) (Exp (Cont String))
          | Set_grid (Maybe Integer) (Exp GridTerm)
          | Page_dflt (Exp Dimension) (Exp Length) (Exp Length)
          | Grid_dflt (Exp GridTerm)
          | Newpage_dflt
          | Newpage (Exp Dimension) (Exp Dimension)
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


