
predef :: Int
predef = 65535

main :: IO ()
main = do putStrLn "Adivine el numero magico!"
          guessing

guessing :: IO ()
guessing = do guess <- getLine
              intGuess <- return $ read guess
              if intGuess == predef
                then putStrLn "Acertaste!"
                else if intGuess < predef
                then do putStrLn "El numero que buscas es mayor"
                        guessing
                else do putStrLn "El numero que buscas es menor"
                        guessing
