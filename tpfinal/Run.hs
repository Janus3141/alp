module Run where


import Control.Monad
import Graphics.PDF
import Types



--------- PASAR A PROCESADOR DE LENGUAJE -----------------------------------
------------------------------
--- Transiciones de fuente ---
------------------------------
{-
toBold :: FontName -> FontName
toBold Helvetica         = Helvetica_Bold
toBold Helvetica_Oblique = Helvetica_BoldOblique
toBold Times_Roman       = Times_Bold
toBold Times_Italic      = Times_BoldItalic
toBold Courier           = Courier_Bold
toBold Courier_Oblique   = Courier_BoldOblique
toBold fn                = fn


toItalics :: FontName -> FontName
toItalics Helvetica      = Helvetica_Oblique
toItalics Helvetica_Bold = Helvetica_BoldOblique
toItalics Times_Roman    = Times_Italic
toItalics Times_Bold     = Times_BoldItalic
toItalics Courier        = Courier_Oblique
toItalics Courier_Bold   = Courier_BoldOblique
toItalics fn             = fn


fromBold :: FontName -> FontName
fromBold Helvetica_Bold        = Helvetica
fromBold Helvetica_BoldOblique = Helvetica_Oblique
fromBold Times_Bold            = Times_Roman
fromBold Times_BoldItalic      = Times_Italic
fromBold Courier_Bold          = Courier
fromBold Courier_BoldOblique   = Courier_Oblique
fromBold fn                    = fn


fromItalics :: FontName -> FontName
fromItalics Helvetica_Oblique     = Helvetica
fromItalics Helvetica_BoldOblique = Helvetica_Bold
fromItalics Times_Italic          = Times_Roman
fromItalics Times_BoldItalic      = Times_Bold
fromItalics Courier_Oblique       = Courier
fromItalics Courier_BoldOblique   = Courier_Bold
fromItalics fn                    = fn



--------- PASAR A PROCESADOR DE LENGUAJE -----------------------------------
-----------------------------
--- Atomizacion de tokens ---
-----------------------------

-- Para separar el texto en lineas que entren en los rectangulos
-- es necesario que un token TextT contenga solo una palabra.
-- Los token para letra negrita e italica pueden traducirse a TextFont.

atomizeToks :: FontName -> [TextTok] -> [TextTok]
atomizeToks _ []     = []
atomizeToks f (x:xs) = case x of
    TextT w     -> (map TextT $ words w) ++ atomizeToks f xs
    TextBold    -> changeFont $ toBold f
    TextBoldOff -> changeFont $ fromBold f
    TextItalics -> changeFont $ toItalics f
    TextItalOff -> changeFont $ fromItalics f
    _           -> x:(atomizeToks f xs)
    where changeFont f' = (TextFont f') : (atomizeToks f' xs)

-}


----------------------------
--- Separacion en lineas ---
----------------------------

justify :: AlignFunction
justify _ waste wrdCnt end = [TextWordSpa addedSpace]
    where addedSpace = if wrdCnt <= 1 || end then 0 else waste / (wrdCnt-1)


flushLeft :: AlignFunction
flushLeft _ _ _ _ = []


flushRight :: AlignFunction
flushRight f waste _ _ = [TextFont Helvetica, TextSize 1] ++
                         [TextT prefixSpace] ++
                         [TextFont fn, TextSize fs]
    where PDFFont fn fs = f
          minFont = PDFFont Helvetica 1
          minSpaWdt = charWidth minFont ' '
          spaceNum = round (waste / minSpaWdt)
          prefixSpace = ' ' <$ [1..spaceNum]



-- Encontrar la altura de una linea de texto

lead :: PDFFont -> Text -> PDFFloat
lead f []     = getHeight f
lead f@(PDFFont n s) (t:ts) = case t of
    TextFont n'   -> comp $ PDFFont n' s
    TextSize s'   -> comp $ PDFFont n s'
    _             -> lead f ts
    where comp f' = max (getHeight f) (lead f' ts)



-- Buscar ultimo comando de espaciado interlinear en la
-- linea dada. Si no hay, se devuelve Nothing

lineSpace :: Text -> Maybe PDFFloat
lineSpace [] = Nothing
lineSpace ((TextLnSpace sp):xs) = mplus (lineSpace xs) (Just sp)
lineSpace (x:xs) = lineSpace xs



-- Toma la fuente inicial, una lista de tokens y el ancho del rectangulo.
-- Devuelve la ultima fuente encontrada, una linea de texto acotada por el
-- ancho dado, el espacio sobrante de la linea, la cantidad de palabras que
-- entraron, el texto que no entro y un booleano que indica True si la linea
-- termino antes de que el algoritmo termine (es decir, si se encuentra un
-- retorno de linea o no hay mas texto).

lineFit :: PDFFont -> Text -> PDFFloat ->
           (PDFFont, Text, PDFFloat, PDFFloat, Text, Bool)
lineFit f [] spare     = (f, [], spare, 0, [], True)
lineFit f@(PDFFont n s) (x:xs) spare = case x of
    TextT w     -> if fWdt w > spare
                   then (f, [TextNewLine], spare, 0, (x:xs), False)
                   else let spare' = spare - fWdt (w ++ " ")
                            (f',prf,waste,wrdCnt,suff,le) = lineFit f xs spare'
                        in (f', (x:prf), waste, wrdCnt+1, suff, le)
    TextNewLine -> (f, [x], spare, 0, xs, True)
    TextFont n' -> chgFont $ PDFFont n' s
    TextSize s' -> chgFont $ PDFFont n s'
    _           -> chgFont $ f
    where fWdt = \s -> textWidth f $ toPDFString s
          chgFont z = let (f', prf, waste, wrdCnt, suff, le) = lineFit z xs spare
                      in (f', (x:prf), waste, wrdCnt, suff, le)



-- Agregar lineas de texto mientras entren (verticalmente) en el rectangulo.
-- Se recibe una funcion de alineacion, la fuente a utilizar al principio,
-- los tokens de todo el texto y la altura y ancho del rectangulo contenedor.
-- Se devuelve la fuente inicial del texto que no entro en el rectangulo, el
-- texto que si entro, el que no, y la altura de la primera linea de texto,
-- en ese orden.

hgtControl :: AlignFunction -> TextState -> PDFFloat -> PDFFloat ->
              (Text,Text,PDFFloat)
hgtControl _ (TextState [] _ _) _ _ = ([],[],0)
hgtControl fun txt hgt wdt = if wrdCnt <= 0
                             then ([],[],0)
                             else if hgt - lineHgt < 0
                             then ([], [TextFont f, TextSize s, TextLnSpace ls] ++ toks, 0)
                             else (line' ++ pref, suff, lineHgt)
    where TextState toks fnt ls = txt
          (fnt', line, waste, wrdCnt, rest, end) = lineFit fnt toks wdt
          Just lineSpa = mplus (lineSpace line) (Just ls)
          lineHgt = (lead fnt line) + lineSpa
          txt' = TextState rest fnt' lineSpa
          (pref,suff,nextHgt) = hgtControl fun txt' (hgt-lineHgt) wdt
          line' = (fun fnt waste wrdCnt end) ++ [TextLead nextHgt] ++ line
          PDFFont f s = fnt



align :: Alignment -> TextState -> PDFFloat -> PDFFloat ->
         (Text, Text, PDFFloat)
align x = hgtControl fun
    where fun = case x of
                    FlushedRight -> flushRight
                    FlushedLeft  -> flushLeft
                    Justified    -> justify





------------------------
--- Dibujo de un PDF ---
------------------------

-- Dibujar paginas del PDF
-- Se toma el documento que se desea escribir (estructura de cada pagina y texto),
-- y la estructura de una pagina por defecto. Si el texto no cabe en las paginas
-- dadas, se crean mas con la estructura adicional hasta utilizarse todo el texto.

drawPDF :: PDFDoc -> Page [Rect] -> PDF ()
drawPDF ([], []) _         = return ()
drawPDF (pages,txt) def_page = case pages of
                                []     -> do txt' <- drawPage def_page
                                             drawPDF ([],txt') def_page
                                (p:ps) -> do txt' <- drawPage p
                                             drawPDF (ps,txt') def_page
    where drawPage (Page (x,y) r) = do let rect = Just $ PDFRect 0 0 x y
                                       page <- addPage rect
                                       drawRects page r txt




-- Dibujar rectangulos de una pagina

drawRects :: PDFReference PDFPage -> [Rect] -> Text -> PDF Text
drawRects page [] txt     = return txt
drawRects page (x:xs) txt = case cont of
    Cont_empty              -> next txt
    Cont_body algmnt        -> do txt' <- dwp $ drawText $ drawTextToks pos' algmnt txt
                                  next txt'
    Cont_float_txt algmnt t -> do dwp $ drawText $ drawTextToks pos' algmnt t
                                  next txt
    Cont_image file         -> do let imgSz = jpegBounds file
                                  img <- createPDFJpeg file
                                  dwp $ withNewContext $ drawImage pos' imgSz img 
                                  next txt
    where Rect pos (padx,pady) edgs cont comm = x
          pad = (fromIntegral padx) :+ (fromIntegral pady) :: Point
          pos' = ((fst pos) + pad, (snd pos) - pad)
          dwp = drawWithPage page
          next t = do dwp $ drawComment pos' comm
                      dwp $ drawEdges pos (fst edgs)
                      dwp $ drawEdges pos' (snd edgs)
                      drawRects page xs t



-- Dibujar texto en un rectangulo.
-- Se recibe la posicion donde se debe escribir, la alineacion requerida y el texto.
-- Se devuelve el texto que no entro en el rectangulo.

drawTextToks :: (Point,Point) -> Alignment -> Text -> PDFText Text
drawTextToks pos algmt []   = return []
drawTextToks pos algmt toks = do setFont font
                                 textStart x1 (y2 - lead)
                                 renderMode FillText
                                 leading lead
                                 write
                                 return rest
    where [TextFont f, TextSize s] = take 2 toks
          font = PDFFont f s
          ((x1 :+ y1),(x2 :+ y2)) = pos
          width = x2 - x1
          height = y2 - y1
          txtSt = TextState (drop 2 toks) font 0
          (aligned,rest,lead) = align algmt txtSt height width
          write = drawTok font aligned



-- Procesamiento de tokens de texto.
-- Se recibe la fuente inicial, la lista de tokens (Text) y el ancho escrito
-- hasta el momento en la linea (inicialmente 0).

drawTok :: PDFFont -> Text -> PDFText ()
drawTok _ []                   = return ()
drawTok f@(PDFFont n s) (t:ts) = case t of
    TextT w      -> do let w' = toPDFString $ w ++ " "
                       displayText w'
                       drawTok f ts
    TextFont n'  -> changeFont $ PDFFont n' s
    TextSize s'  -> changeFont $ PDFFont n s'
    TextNewLine  -> do startNewLine
                       drawTok f ts
    TextWordSpa sp  -> do wordSpace sp
                          drawTok f ts
    TextLead l      -> do leading l
                          drawTok f ts
    _               -> drawTok f ts
    where changeFont font = do setFont font
                               drawTok font ts
          width str = textWidth f str




-- Dibujar una imagen en un rectangulo

drawImage :: (Point,Point) -> (Int,Int) -> PDFReference PDFJpeg -> Draw ()
drawImage (p1,p2) (w,h) jpg = do applyMatrix mtx
                                 drawXObject jpg
    where -- Tamaño original de la imagen
          imgWdt = (fromIntegral w) :: PDFFloat
          imgHgt = (fromIntegral h) :: PDFFloat
          -- Tamaño del rectangulo
          (x :+ y) = p1
          rectWdt = realPart p2 - x
          rectHgt = imagPart p2 - y
          -- Matriz para acomodar posicion y tamaño
          mtx = Matrix (rectWdt/imgWdt) 0 0 (rectHgt/imgHgt) x y


-- Dibujar bordes de un rectangulo

drawEdges :: (Point,Point) -> Edges -> Draw ()
drawEdges (p1,p2) (u,d,l,r) = drawE1 >> drawE2 >> drawE3 >> drawE4
    where (x1 :+ y1) = p1
          (x2 :+ y2) = p2
          drawE1 = if u then stroke $ Line x1 y2 x2 y2 else emptyDrawing
          drawE2 = if d then stroke $ Line x1 y1 x2 y1 else emptyDrawing
          drawE3 = if l then stroke $ Line x1 y1 x1 y2 else emptyDrawing
          drawE4 = if r then stroke $ Line x2 y1 x2 y2 else emptyDrawing


-- Dibujar un comentario

drawComment :: (Point,Point) -> Comment -> Draw ()
drawComment _ Nothing      = emptyDrawing
drawComment pos (Just cmt) = newAnnotation cmt'
    where ((x1 :+ y1),(x2 :+ y2)) = pos
          cmt' = TextAnnotation (toPDFString cmt) [x1,y1,x2,y2] Help







t :: String
t = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod\
    \ tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,\
    \ quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo\
    \ consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse\
    \ cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat\
    \ non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."



try :: IO ()
try = do Right jpg1 <- readJpegFile "1.jpg"
         runPdf "test.pdf" standardDocInfo (PDFRect 0 0 0 0) $ drawPDF pdf def_page
    where edges = ((True,True,True,True), (True,True,True,True))
          c2 = Cont_body FlushedRight
          r1 = Rect ((25 :+ 625), (525 :+ 1225)) (50,50) edges c2 Nothing
          c1 = Cont_body FlushedRight
          r2 = Rect ((25 :+ 25), (525 :+ 625)) (20,10) edges c1 Nothing
          txt1 = concat $ (words t) <$ [1..8]
          toks1 = (map TextT txt1) ++ [TextLnSpace 4] ++ (map TextT txt1)
          toks2 = [TextFont Helvetica, TextSize 12] ++ toks1
          def_page = Page (550,1250) [r1,r2]
          pdf = ([], toks2)


