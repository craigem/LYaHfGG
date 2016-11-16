-- arg-test from Chapter 09

-- import required modules
import System.Environment
import Data.List

main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "The arguments are: "
    mapM putStrLn args
    putStrLn "The program name is: "
    putStrLn progName
