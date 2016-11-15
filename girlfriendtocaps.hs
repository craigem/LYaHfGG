-- girlfiendtocaps from Chapter 09

-- import required modules
import System.IO
import Data.Char

main = do
    contents <- readFile "girlfriend.txt"
    writeFile "girlfriendtocaps.txt" (map toUpper contents)
