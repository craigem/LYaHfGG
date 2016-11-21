-- linecount from Chapter 09

-- import required modules
import System.Environment
import System.IO
import System.Directory

main = do (fileName:_) <- getArgs
          -- Check the file provided actually exists
          fileExists <- doesFileExist fileName
          if fileExists
              then do contents <- readFile fileName
                      putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
              else do putStrLn "The file doesn't!"
