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


-- Modificadores para imagenes. Se puede:
---- Realizar una rotacion en grados.
---- Especificar el tamaño de la imagen, dando ancho y altura.
---- Escalar la imagen (primero eje x, luego y).
---- Indicar que la imagen ocupa todo el rectangulo.
data ImageMod = ImgRotate Integer
              | ImgSize Integer Integer
              | ImgScale Integer Integer
              | ImgFill
    deriving Show


-- Contenido que puede aparecer en un rectangulo.
-- Una imagen lleva su direccion en disco y un conjunto de
-- modificadores a aplicar.
-- El texto es una serie de palabras y modificadores. Cada
-- linea recibe las modificaciones que aparecen antes en
-- la lista.
data Content = Image JpegFile [ImageMod]
             | Text Alignment [TextTok]
             | Empty
    deriving Show


-- Un rectangulo indica su posicion (punto inferior izquierdo,
-- punto superior derecho) y su contenido
data Rect = Rect (Point,Point) Content


-- Cada pagina incluye dos enteros que indican su tamaño en pixeles
-- y el conjunto de rectangulos en que esta dividido.
data Page = Page Int Int [Rect]


-- Un documento es un conjunto de paginas.
type PDFDoc = [Page]


