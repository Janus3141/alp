
module Main where

import System.Exit (exitSuccess,exitFailure)
import System.Environment (getArgs)
import Control.Monad (liftM2)
import Graphics.PDF
import Parser
import Eval
import Run
import Types
import AST
import System.IO


------------------------------------------------------------------------------
------------------ Abrir imagenes dentro de los rectangulos ------------------
------------------------------------------------------------------------------

openImages :: EvalDoc -> IO RunDoc
openImages []     = return []
openImages (p:ps) = do p' <- openImagesPiece p
                       ps' <- openImages ps
                       return (p':ps')
    where openImagesPiece (pages,txt) = do
              pages' <- mapM openImagesPage pages
              return (pages', txt)


openImagesPage :: Page EvalCont -> IO (Page RunCont)
openImagesPage (Page sz rects) = do
        rects' <- mapM openImagesRect rects
        return (Page sz rects')
    where openImagesRect (Rect p m e cont d) = do
              cont' <- openImage cont
              return (Rect p m e cont' d)
          openImage (Cont_image file) = do
              let openFail = fail ("Error opening file " ++ show file)
              img <- readJpegFile file
              either openFail (return . Cont_image) img
          openImage (Cont_body a) = return $ Cont_body a
          openImage (Cont_float_txt a b) = return $ Cont_float_txt a b
          openImage Cont_empty = return Cont_empty





------------------------------------------------------------------------------
---------------------- Manejo de argumentos al programa ----------------------
------------------------------------------------------------------------------

data CallArgs = CallArgs { argDebug :: Bool
                         , argOutName :: String
                         , argFile :: String }
    deriving Show


initArgs :: CallArgs
initArgs = CallArgs False "a.pdf" ""


argParse :: CallArgs -> [String] -> IO CallArgs
argParse args []     = printHelp >> return args
argParse args ["-h"] = printHelp >> return args
argParse args (f:xs) = argParse' (args {argFile = f}) xs
    where argParse' args []       = return args
          argParse' args ("-d":xs) = argParse' (args {argDebug = True}) xs
          argParse' args ("-o":(n:xs)) = argParse' (args {argOutName = n}) xs
          argParse' _ (x:_) = fail ("Invalid argument: " ++ show x)




------------------------------------------------------------------------------
----------------------------- Inicio del programa ----------------------------
------------------------------------------------------------------------------

main :: IO ()
main = do args <- getArgs
          args' <- argParse initArgs args
          if argFile args' == "" then exitSuccess
                                 else return ()
          handle <- openFile (argFile args') ReadMode
          hSetEncoding handle utf8
          content <- hGetContents handle
          parseResult <- return $ parseDoc content
          either print (evaluate args') parseResult


evaluate :: CallArgs -> Doc -> IO ()
evaluate args d = do
        result <- eval (argFile args) d
        (page,doc) <- either printExit return result
        (page',doc') <- liftM2 (,) (openImagesPage page) (openImages doc)
        pdf <- return $ drawPDF doc' page'
        runPdf (argOutName args) standardDocInfo (PDFRect 0 0 0 0) pdf
    where printExit err = print err >> exitFailure


printHelp :: IO [()]
printHelp = mapM putStrLn lns
    where lns = ["Argumentos validos para el programa: "
                , "file    : Archivo a evaluar a PDF. Debe ser\
                  \ el primer argumento."
                , "-o name : Guarda el pdf con el nombre 'name'."
                , "-d      : Activa el modo debug (igual que\
                  \ usar '\\debug')."
                , "-h      : Imprimir este mensaje."
                ]

