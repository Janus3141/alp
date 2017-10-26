
import System.IO
import System.Environment
import Data.Char

main :: IO ()
main = do (from:(to:rest)) <- getArgs
          contents <- readFile $ from
          writeFile to $ map toUpper contents

