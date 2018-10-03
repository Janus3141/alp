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
               | Center
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


type RunCont = Cont JpegFile Text


instance Show (Cont a b) where
    show (Cont_image _)        = "Image"
    show (Cont_body _)         = "Body"
    show (Cont_float_txt _ _)  = "Float text"
    show Cont_empty            = "Empty"


-- Tipo para indicar que bordes de un rectangulo se deben dibujar.
-- Estos se interpretan como (Superior, Inferior, Izquierdo, Derecho).
type Edges = (Bool, Bool, Bool, Bool)

hid :: Edges
hid = (False,False,False,False)


-- Comentarios para debug
type Comment = Maybe String


-- Un rectangulo indica su posicion (punto inferior izquierdo,
-- punto superior derecho), el tamaño de sus margenes en pixeles,
-- los bordes que se deben dibujar (primero externos, luego internos),
-- su contenido y por ultimo un comentario para debug.
data Rect c = Rect (Point,Point) Margins (Edges,Edges) c Comment


instance Show c => Show (Rect c) where
    show (Rect pos mgs _ cont _) = intercalate " " strs
        where strs = ["Rect", show pos, show mgs, show cont]


-- Cada pagina incluye dos enteros que indican su tamaño en pixeles
-- y el conjunto de rectangulos en que esta dividido.
data Page c = Page (Integer,Integer) [Rect c]
    deriving Show


-- Una seccion del documento es un conjunto de paginas dadas manualmente
-- y el texto a insertar.
type PDFPiece c = ([Page c], Text)


-- Un documento es un conjunto de secciones.
type PDFDoc c = [PDFPiece c]


type RunDoc = PDFDoc RunCont

