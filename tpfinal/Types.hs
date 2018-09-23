module Types where

import Graphics.PDF


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
-- El texto es una serie de tokens que se escriben en las paginas
-- creadas en el orden dado (por la lista de rectangulos).
data Content = Image JpegFile
             | Body Alignment
             | Float_text Alignment Text
             | Empty


-- Tipo para indicar que bordes de un rectangulo se deben dibujar.
-- Estos se interpretan como (Superior, Inferior, Izquierdo, Derecho).
type Edges = (Bool, Bool, Bool, Bool)


-- Comentarios para debug
type Comment = Maybe String


-- Un rectangulo indica su posicion (punto inferior izquierdo,
-- punto superior derecho), el tamaño de sus margenes en pixeles,
-- los bordes que se deben dibujar (primero externos, luego internos),
-- su contenido y por ultimo un comentario para debug.
data Rect = Rect (Point,Point) Margins (Edges,Edges) Content Comment


-- Cada pagina incluye dos enteros que indican su tamaño en pixeles
-- y el conjunto de rectangulos en que esta dividido.
data Page = Page (Int,Int) [Rect]


-- Un documento es un conjunto de paginas y el texto que se debe
-- escribir en ellas.
type PDFDoc = ([Page], Text)


