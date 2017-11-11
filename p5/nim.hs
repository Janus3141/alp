
data Table = Table {first :: Int,
                    second :: Int,
                    third :: Int,
                    fourth :: Int,
                    fifth :: Int}


newtype Player = Player Int


printTable :: Table -> IO ()
printTable t = do putStrLn $ "1 : " ++ (stars $ first t)
                  putStrLn $ "2 : " ++ (stars $ second t)
                  putStrLn $ "3 : " ++ (stars $ third t)
                  putStrLn $ "4 : " ++ (stars $ fourth t)
                  putStrLn $ "5 : " ++ (stars $ fifth t)
              where stars 0 = []
                    stars x = "*" ++ stars (x-1)

play :: Table -> IO Table
play t = do row <- getLine
            amount <- getLine
            let amnt = read amount in
                case read row of
                   1 -> decide first 1 amnt
                   2 -> decide second 2 amnt
                   3 -> decide third 3 amnt
                   4 -> decide fourth 4 amnt
                   5 -> decide fifth 5 amnt
                   _ -> invalid
        where decide f r a = if (f t) >= a && a > 0
                             then return $ case r of
                                            1 -> Table (fir-a) se th fo fi
                                            2 -> Table fir (se-a) th fo fi
                                            3 -> Table fir se (th-a) fo fi
                                            4 -> Table fir se th (fo-a) fi
                                            5 -> Table fir se th fo (fi-a)
                             else invalid
              invalid = do putStrLn "Jugada invalida"
                           putStrLn "Introduzca otro numero de linea y"
                           putStrLn "otro numero de estrellas a quitar"
                           play t
              fir = first t
              se = second t
              th = third t
              fo = fourth t
              fi = fifth t


isEmpty :: Table -> Bool
isEmpty (Table a b c d e) = a+b+c+d+e == 0


loop :: Player -> Table -> IO ()
loop (Player i) t = do printTable t
                       t' <- play t
                       if isEmpty t'
                         then putStrLn $ "El ganador es Jugador " ++ show i ++ "!"
                         else do putStrLn $ "Turno del jugador " ++ show j
                                 loop (Player j) t'
                   where j = (mod i 2) + 1

main :: IO ()
main = do putStrLn "Este es nim"
          putStrLn "Dos jugadores se turnan para sacar una o mas estrellas"
          putStrLn "de alguna fila. El ganador es el jugador que saca"
          putStrLn "la ultima estrella. Cuando se indique, el jugador debe"
          putStrLn "introducir un numero de fila, presionar enter e"
          putStrLn "introducir la cantidad de estrellas que desea sacar"
          putStrLn "de esa fila seguido por otro enter. Si la fila no"
          putStrLn "es un numero entre 1 y 5, o si no hay tantas estrellas"
          putStrLn "en esa fila como se quieren sacar, el jugador debe"
          putStrLn "introducir nuevamente otros numeros correctos."
          putStrLn "Empieza el jugador 1!"
          loop (Player 1) (Table 5 4 3 2 1)

