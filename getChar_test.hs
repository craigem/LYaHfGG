-- getChar test from Chapter 09

-- Import required modules
import Control.Monad

main = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main
