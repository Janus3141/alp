module Main where

import System.Environment (getArgs,getProgName)
import Parser(parser)
-- El modulo Parser tambien exporta una funcion
--
--   parser :: String -> Comm
--
-- que convierte una cadena de caracteres que representa un programa LIS en una
-- expresion de tipo Comm.


-- Modificar este import para usar diferentes evaluadores
import Eval3
---------------------------------------------------------

main :: IO ()
main = do args <- getArgs
          case args of
             []      -> printHelp
             (arg:_) -> run arg

-- Ejecuta un programa a partir de su archivo fuente
run :: [Char] -> IO ()
run ifile =
    do
    s <- readFile ifile
    (print . topp  . eval) (parser s)

printHelp :: IO ()
printHelp = do name <- getProgName
               if name /= "<interactive>" then
                  putStrLn ("Intérprete de LIS (TP4).\n" ++
                         "Pase como argumento el nombre del archivo a ejecutar.\n"++
                         "Por ejemplo: "++name++" sqrt.lis\n")
                                         else
                  putStrLn ("Intérprete de LIS (TP4) en modo interactivo.\n" ++
                         "Pase como argumento a la función run "++
                         "una cadena con el nombre del archivo a ejecutar.\n"++
                         "Por ejemplo: run \"sqrt.lis\"\n")
