module Types where

import Graphics.PDF



-- Texto y sus modificadores
data TextTok = TextWord String
             | TextFont FontName
             | TextSize Int
             | TextBold
             | TextItalics
             | TextNewLine
             | TextWordSpa PDFFloat
    deriving Show


data Alignment = FlushedRight
               | FlushedLeft
               | Justified
    deriving Show


-- Contenido que puede aparecer en un rectangulo.
-- Una imagen lleva, ademas del archivo, dos enteros que
-- indican el tamaño de sus margenes en pixeles.
-- El texto es una serie de palabras y modificadores. Cada
-- linea recibe las modificaciones que aparecen antes en
-- la lista.
data Content = Image JpegFile (Int,Int)
             | Text Alignment [TextTok]
             | Empty


-- Tipo para indicar que bordes de un rectangulo se deben dibujar.
-- Estos se interpretan como (Superior, Inferior, Izquierdo, Derecho).
type Edges = (Bool, Bool, Bool, Bool)

-- Un rectangulo indica su posicion (punto inferior izquierdo,
-- punto superior derecho) y su contenido
data Rect = Rect (Point,Point) Content


-- Cada pagina incluye dos enteros que indican su tamaño en pixeles
-- y el conjunto de rectangulos en que esta dividido.
data Page = Page (Int,Int) [Rect]


-- Un documento es un conjunto de paginas.
type PDFDoc = [Page]


