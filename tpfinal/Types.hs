module Types where

import Graphics.PDF
import Data.List (intercalate)


-- Tamaño de margenes en pixeles. Cada uno se aplica en igual medida en ambos lados
-- opuestos. El primer entero indica la medida en el eje x.
type Margins = (Integer,Integer)


-- Texto y sus modificadores
data TextTok = TextT String
             | TextFont FontName
             | TextSize Int
             | TextNewLine
             | TextWordSpa PDFFloat
             | TextLead PDFFloat
             | TextLnSpace PDFFloat
    deriving Show


data TextState = TextState { ts_txt :: Text
                           , ts_font :: PDFFont
                           , ts_lnSpace :: PDFFloat
                           }


type Text = [TextTok]


data Alignment = FlushedRight
               | FlushedLeft
               | Justified
    deriving Show



type AlignFunction = PDFFont -> PDFFloat -> PDFFloat -> Bool -> Text


-- Contenido que puede aparecer en un rectangulo.
-- Se parametrizan las imagenes ya que primero llevaran el nombre del
-- archivo al parsear, y luego el archivo abierto (JpegFile). El
-- texto 'flotante' se parametriza para llevar primero String y luego Text.
data Cont a b = Cont_image a
              | Cont_body Alignment
              | Cont_float_txt Alignment b
              | Cont_empty


instance Show (Cont a b) where
    show (Cont_image _)        = "Image"
    show (Cont_body _)         = "Body"
    show (Cont_float_txt _ _) = "Float text"
    show Cont_empty            = "Empty"


-- Tipo para indicar que bordes de un rectangulo se deben dibujar.
-- Estos se interpretan como (Superior, Inferior, Izquierdo, Derecho).
type Edges = (Bool, Bool, Bool, Bool)


-- Comentarios para debug
type Comment = Maybe String


-- Un rectangulo indica su posicion (punto inferior izquierdo,
-- punto superior derecho), el tamaño de sus margenes en pixeles,
-- los bordes que se deben dibujar (primero externos, luego internos),
-- su contenido y por ultimo un comentario para debug.
data Rect = Rect (Point,Point) Margins (Edges,Edges) (Cont JpegFile Text) Comment


instance Show Rect where
    show (Rect pos mgs _ cont _) = intercalate " " strs
        where strs = ["Rect", show pos, show mgs, show cont]


-- Cada pagina incluye dos enteros que indican su tamaño en pixeles
-- y el conjunto de rectangulos en que esta dividido.
data Page r = Page (Integer,Integer) r
    deriving Show


-- Un documento es un conjunto de paginas y el texto que se debe
-- escribir en ellas.
type PDFDoc = ([Page [Rect]], Text)


