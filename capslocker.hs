-- capslocker.hs from Chapter 09

-- import required modules:
import Data.Char

main = do
    contents <- getContents
    putStrLn (map toUpper contents)
