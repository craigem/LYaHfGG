-- Hello, from Chapter 09

main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++", you rock!")
