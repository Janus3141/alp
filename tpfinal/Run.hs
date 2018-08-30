module Run where


import Graphics.PDF
import Types


------------------------------
--- Transiciones de fuente ---
------------------------------

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



------------------------------------
--- Atomizacion de tokens TextWord ---
------------------------------------

-- Atomizar tokens de texto. Para separar el texto en lineas
-- que entren en los rectangulos es necesario que un token
-- contenga solo una palabra.

atomizeToks :: [TextTok] -> [TextTok]
atomizeToks [] = []
atomizeToks (x:xs) = case x of
    TextWord w -> (map TextWord $ words w) ++ atomizeToks xs
    _          -> x:(atomizeToks xs)



------------------------------------------
--- Funciones para alineacion de lineas ---
------------------------------------------

justify :: PDFFont -> [TextTok] -> PDFFloat -> [TextTok]
justify _ [] _     = []
justify f toks wdt = if wrdCnt == 0 then []
                     else let rest = justify f' suff wdt
                              addedSpace = if wrdCnt == 1 || parEnd then 0
                                           else waste / (wrdCnt-1)
                              spacedPref = (TextWordSpa addedSpace):pref
                          in spacedPref ++ rest
    where (f', pref, waste, wrdCnt, suff, parEnd) = lineFit f toks wdt



flushLeft :: PDFFont -> [TextTok] -> PDFFloat -> [TextTok]
flushLeft _ [] _     = []
flushLeft f toks wdt = if wrdCnt == 0 then []
                       else let rest = flushLeft f' suff wdt
                            in pref ++ rest
    where (f', pref, _, wrdCnt, suff, _) = lineFit f toks wdt



flushRight :: PDFFont -> [TextTok] -> PDFFloat -> [TextTok]
flushRight _ [] _ = []
flushRight f toks wdt = if wrdCnt == 0 then []
                        else let rest = flushRight f' suff wdt
                                 spaceNum = round (waste / charWidth f' ' ')
                                 neededSpa = [' ' | _ <- [1..spaceNum]]
                             in (TextWord neededSpa):pref ++ rest
    where (f', pref, waste, wrdCnt, suff, _) = lineFit f toks wdt



align :: Alignment -> PDFFont -> [TextTok] -> PDFFloat -> [TextTok]
align FlushedRight = flushRight
align FlushedLeft  = flushLeft
align Justified    = justify


--------------------------------------
--- Separacion en lineas del texto ---
--------------------------------------

lineFit :: PDFFont -> [TextTok] -> PDFFloat ->
           (PDFFont, [TextTok], PDFFloat, PDFFloat, [TextTok], Bool)
lineFit f [] spare     = (f, [], spare, 0, [], True)
lineFit f (x:xs) spare = case x of
    TextWord w -> let wwdt = textWidth f $ toPDFString w in
                  if wwdt > spare
                  then (f, [TextNewLine], spare, 0, (x:xs), False)
                  else let spare' = spare - (textWidth f $ toPDFString (w ++ " "))
                           (f',prf,waste,wrdCnt',suff,pe) = lineFit f xs spare'
                       in (f', (x:prf), waste, wrdCnt'+1, suff, pe)
    TextNewLine -> (f, [x], spare, 0, xs, True)
    TextFont n  -> let PDFFont _ s = f
                       nf = PDFFont n s
                       (f', prf, waste, wrdCnt, suff, pe) = lineFit nf xs spare
                   in (f', (x:prf), waste, wrdCnt, suff, pe)
    TextSize s  -> let PDFFont n _ = f
                       sf = PDFFont n s
                       (f', prf, waste, wrdCnt, suff, pe) = lineFit sf xs spare
                   in (f', (x:prf), waste, wrdCnt, suff, pe)
    _           -> let (f', prf, waste, wrdCnt, suff, pe) = lineFit f xs spare
                   in (f', (x:prf), waste, wrdCnt, suff, pe)



------------------------------------
--- Dibujado de un PDF ---
------------------------------------

-- Dibujar paginas del PDF

drawPDF :: PDFDoc -> PDF ()
drawPDF [] = return ()
drawPDF ((Page (x,y) r):xs) = do let rect = Just $ PDFRect 0 0 x y
                                 page <- addPage rect
                                 drawRects page r
                                 drawPDF xs


-- Dibujar rectangulos de una pagina

drawRects :: PDFReference PDFPage -> [Rect] -> PDF ()
drawRects page []                   = drawWithPage page emptyDrawing
drawRects page ((Rect pos cont):xs) = case cont of
    Empty            -> do dwp emptyDrawing
                           next
    Text algmnt toks -> do dwp $ drawText $ drawTextToks pos algmnt toks
                           next
    Image file pads  -> do let imgSz = jpegBounds file
                           img <- createPDFJpeg file
                           dwp $ withNewContext $ drawImage pos imgSz img pads
                           next
    where dwp = drawWithPage page
          next = drawRects page xs


-- Dibujar texto en un rectangulo

drawTextToks :: (Point,Point) -> Alignment -> [TextTok] -> PDFText ()
drawTextToks (x,y) algmt toks = do setFont font
                                   textStart (realPart x) (imagPart y - fontHgt)
                                   renderMode FillText
                                   leading fontHgt
                                   drawTok height font aligned 0
    where [TextFont name, TextSize n] = take 2 toks
          font = PDFFont name n
          fontHgt = getHeight font
          width = realPart y - realPart x
          height = imagPart y - imagPart x
          atomToks = atomizeToks $ drop 2 toks
          aligned = align algmt font atomToks width


-- Procesamiento de tokens de texto

drawTok :: PDFFloat ->      -- Altura del rectangulo
           PDFFont ->
           [TextTok] ->
           PDFFloat ->      -- Cantidad de lineas escritas
           PDFText ()
drawTok  _ _ [] _                    = emptyText
drawTok y f@(PDFFont n s) (z:zs) hgt = case z of
    TextWord w  -> do displayText (toPDFString $ w ++ " ")
                      drawTok y f zs hgt
    TextFont n' -> changeFont $ PDFFont n' s
    TextSize s' -> changeFont $ PDFFont n s'
    TextBold    -> changeFont $ PDFFont (toBold n) s
    TextItalics -> changeFont $ PDFFont (toItalics n) s
    TextNewLine -> let fontHgt = getHeight f
                   in if hgt < y
                      then do startNewLine
                              drawTok y f zs (hgt+fontHgt)
                      else emptyText
    TextWordSpa sp -> do wordSpace sp
                         drawTok y f zs hgt
    where changeFont font = do setFont font
                               leading $ getHeight font
                               drawTok y font zs hgt


emptyText :: PDFText ()
emptyText = displayText $ toPDFString ""


-- Dibujar una imagen en un rectangulo

drawImage :: (Point,Point) ->
             (Int,Int) ->
             PDFReference PDFJpeg ->
             (Int,Int) ->
             Draw ()
drawImage (p1,p2) imgSz jpg (px,py) = do applyMatrix mtx
                                         drawXObject jpg
    where -- Tamaño original de la imagen
          (imgWdt',imgHgt') = imgSz
          imgWdt = (fromIntegral imgWdt') :: PDFFloat
          imgHgt = (fromIntegral imgHgt') :: PDFFloat
          -- Tamaño de los margenes (padding)
          padx = (fromIntegral px) :: PDFFloat
          pady = (fromIntegral py) :: PDFFloat
          -- Coordenadas del punto inferior izq sumando padding
          x = min (realPart p1 + padx) (realPart p2 - padx)
          y = min (imagPart p1 + pady) (imagPart p2 - pady)
          -- Tamaño final de la imagen contando margenes
          rectWdt = realPart p2 - padx - x
          rectHgt = imagPart p2 - pady - y
          -- Matriz para acomodar posicion y tamaño
          mtx = Matrix (rectWdt/imgWdt) 0 0 (rectHgt/imgHgt) x y





c1 :: Content
c1 = Text FlushedRight [TextFont Courier, TextSize 12, TextWord t]

c2 :: Content
c2 = Text FlushedRight [TextFont Helvetica, TextSize 20, TextWord t]

t :: String
t = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

try :: IO ()
try = do let r1 = Rect ((25 :+ 625), (525 :+ 1225))
             r2 = Rect ((25 :+ 25), (525 :+ 625)) c2
         Right jpg1 <- readJpegFile "1.jpg"
         runPdf "test.pdf" standardDocInfo (PDFRect 0 0 0 0) $ drawPDF [Page (550,1250) [r1 (Image jpg1 (30,30)),r2]]
