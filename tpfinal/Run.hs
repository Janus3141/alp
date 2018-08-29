module Run where


import Graphics.PDF
import Types



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


atomizeToks :: [TextTok] -> [TextTok]
atomizeToks [] = []
atomizeToks (x:xs) = case x of
    TextWord w -> (map TextWord $ words w) ++ atomizeToks xs
    _          -> x:(atomizeToks xs)


justify :: PDFFont -> [TextTok] -> PDFFloat -> [TextTok]
justify _ [] _     = []
justify f toks wdt = let (f', pref, prWdt, wrdCnt, suff) = lineFit f toks wdt 0
                     in if wrdCnt == 0 then []
                        else let rest = justify f' suff wdt
                                 addedSpace = case wrdCnt of
                                                1 -> 0
                                                _ -> (wdt-prWdt) / (wrdCnt-1)
                                 spacedPref = (TextWordSpa addedSpace):pref
                             in spacedPref ++ rest


flushLeft :: PDFFont -> [TextTok] -> PDFFloat -> [TextTok]
flushLeft _ [] _     = []
flushLeft f toks wdt = let (f', pref, _, wrdCnt, suff) = lineFit f toks wdt 0
                        in if wrdCnt == 0 then []
                           else let rest = flushLeft f' suff wdt
                                in pref ++ rest


flushRight :: PDFFont -> [TextTok] -> PDFFloat -> [TextTok]
flushRight _ [] _ = []
flushRight f toks wdt = let (f', pref, prWdt, wrdCnt, suff) = lineFit f toks wdt 0
                        in if wrdCnt == 0 then []
                           else let rest = flushRight f' suff wdt
                                    trailWdt = wdt - prWdt
                                    spaceNum = round (trailWdt / charWidth f' ' ')
                                    neededSpa = [' ' | _ <- [1..spaceNum]]
                                in (TextWord neededSpa):pref ++ rest


lineFit :: PDFFont -> [TextTok] -> PDFFloat -> PDFFloat ->
           (PDFFont, [TextTok], PDFFloat, PDFFloat, [TextTok])
lineFit f [] wdt prfWdt     = (f, [], wdt, 0, [])
lineFit f (x:xs) wdt prfWdt = case x of
    TextWord w -> let wwdt = textWidth f $ toPDFString w in
                  if wwdt + prfWdt > wdt
                  then (f, [TextNewLine], prfWdt, 0, (x:xs))
                  else let totalWdt = prfWdt + textWidth f $ toPDFString (w ++ " ")
                           (f',prf,prfWdt',wrdCnt',suff) = lineFit f xs wdt totalWdt
                       in (f', (x:prf), prfWdt', wrdCnt'+1, suff)
    TextNewLine -> (f, [x], wdt, 0, xs)
    TextFont n  -> let PDFFont _ s = f
                       nf = PDFFont n s
                       (f', prf, prfWdt', wrdCnt, suff) = lineFit nf xs wdt prfWdt
                   in (f', (x:prf), prfWdt', wrdCnt, suff)
    TextSize s  -> let PDFFont n _ = f
                       sf = PDFFont n s
                       (f', prf, prfWdt', wrdCnt, suff) = lineFit sf xs wdt prfWdt
                   in (f', (x:prf), prfWdt', wrdCnt, suff)
    _           -> let (f', prf, prfWdt', wrdCnt, suff) = lineFit f xs wdt prfWdt
                   in (f', (x:prf), prfWdt', wrdCnt, suff)


align :: Alignment -> PDFFont -> [TextTok] -> PDFFloat -> [TextTok]
align FlushedRight = flushRight
align FlushedLeft  = flushLeft
align Justified    = justify


emptyText :: PDFText ()
emptyText = displayText $ toPDFString ""


applyMods :: [ImageMod] -> PDFFloat -> PDFFloat -> PDFFloat -> PDFFloat -> Draw ()
applyMods [] _ _ _ _                           = emptyDrawing
applyMods (x:xs) imgWdt imgHgt rectWdt rectHgt = applyMatrix matrix >> rest
    where rest = applyMods xs imgWdt imgHgt rectWdt rectHgt
          matrix = case x of
            ImgRotate ang -> rotate $ Degree $ fromIntegral ang
            ImgSize x y   -> let float_x = fromIntegral x :: PDFFloat
                                 float_y = fromIntegral y :: PDFFloat
                                 x' = min rectWdt float_x
                                 y' = min rectHgt float_y
                             in scale (x'/imgWdt) (y'/imgHgt)
            ImgScale x y  -> let float_x = fromIntegral x :: PDFFloat
                                 float_y = fromIntegral y :: PDFFloat
                                 x' = min rectWdt (x' * imgWdt)
                                 y' = min rectHgt (y' * imgHgt)
                             in scale x' y'
            ImgFill       -> scale (rectWdt/imgWdt) (rectHgt/imgHgt)


drawPDF :: PDFDoc -> PDF ()
drawPDF [] = return ()
drawPDF ((Page x y r):xs) = do let rect = Just $ PDFRect 0 0 x y
                               page <- addPage rect
                               drawWithPage page $ drawRects r
                               drawPDF xs


drawRects :: [Rect] -> Draw ()
drawRects [] = emptyDrawing
drawRects ((Rect pos cont):xs) = drw >> drawRects xs
    where drw = case cont of
                    Empty           -> emptyDrawing
                    Text algmt toks -> drawText $ drawTextToks pos algmt toks
                    Image file mods  -> drawImage pos dir mods


drawTextToks :: (Point,Point) -> Alignment -> [TextTok] -> PDFText ()
drawTextToks (x,y) algmt toks = do let [TextFont name, TextSize n] = take 2 toks
                                       font = PDFFont name n
                                       fontHgt = getHeight font
                                       width = realPart y - realPart x
                                       height = imagPart y - imagPart x
                                       atomToks = atomizeToks $ drop 2 toks
                                       aligned = align algmt font atomToks width
                                   setFont font
                                   textStart (realPart x) (imagPart y - fontHgt)
                                   renderMode FillText
                                   leading (getHeight font)
                                   drawTok height font aligned 0


drawTok :: PDFFloat ->      -- Altura del rectangulo
           PDFFont ->
           [TextTok] ->
           PDFFloat ->      -- Cantidad de lineas escritas
           PDFText ()
drawTok  _ _ [] _          = emptyText
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


drawImage :: (Point,Point) -> JpegFile -> [ImageMod] -> Draw ()
drawImage (x,y) jpgf mods = do let (imgWdt',imgHgt') = jpegBounds jpgf
                                   imgWdt = (fromIntegral imgWdt') :: PDFFloat
                                   imgHgt = (fromIntegral imgHgt') :: PDFFloat
                                   rectWdt = realPart y - realPart x
                                   rectHgt = imagPart y - imagPart x
                               img <- createPDFJpeg jpgf
                               applyMods mods imgWdt imgHgt rectWdt rectHgt
                               drawXObject img



test :: PDFDoc
test = let r1 = Rect ((25 :+ 625), (525 :+ 1225)) c1
           r2 = Rect ((25 :+ 25), (525 :+ 625)) c2
       in [Page 550 1250 [r1,r2]]


c1 :: Content
c1 = Text FlushedRight [TextFont Courier, TextSize 12, TextWord t]

c2 :: Content
c2 = Text FlushedLeft [TextFont Helvetica, TextSize 30, TextWord t]


t :: String
t = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod
     tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
     quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
     consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
     cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat
     non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."


try :: IO ()
try = runPdf "test.pdf" standardDocInfo (PDFRect 0 0 0 0) $ drawPDF test
