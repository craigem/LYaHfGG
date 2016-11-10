-- getChar test from Chapter 09

main = do
    c <- getChar
    if c /= ' '
        then do
            putChar c
            main
        else return ()
