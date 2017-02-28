-- fmapping example from Chapter 11

-- import required modules
import Data.Char
import Data.List

-- Use function composition to reverse the words
main = do line <- fmap (intersperse '-' . reverse .map toUpper) getLine
          putStrLn line
