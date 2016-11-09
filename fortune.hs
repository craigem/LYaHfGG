-- Fortune from Chapter 09

main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $
        "Read this carefully, because this is your future: " ++
        tellFortune name

tellFortune :: String -> String
tellFortune "Craige" = "the universe loves you"
tellFortune name = "Dogdy, sorry."
