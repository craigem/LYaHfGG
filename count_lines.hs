-- count_lines from Chapter 09

-- import required modules
import System.Environment
import System.IO
import System.IO.Error

-- Book says to use catch but it has been replaced with catchIOError
main = toTry `catchIOError` handler

toTry :: IO ()
toTry = do (fileName:_) <- getArgs
           contents <- readFile fileName
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler e = putStrLn "Whoops, had some trouble!"
