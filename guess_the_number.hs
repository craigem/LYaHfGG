-- guess_the_number from Chapter 09

-- import required modules
import System.Random
import Control.Monad(when)

main = do
    gen <- getStdGen
    askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
    -- Tell the user to guess the number we're thinking of
    putStr "Which number in the range from 1 to 10 am I thinking of? "
    -- get input from stdin and assign to numberString
    numberString <- getLine
    -- Use  when to check if the string the user entered is empty
    when (not $ null numberString) $ do
        let number = read numberString
        -- Compare the input to the random number generated
        if randNumber == number
            -- If they match, send this to stdout
            then putStrLn "You are correct!"
            -- If they don't match, send this to stdout instead.
            else putStrLn $ "Sorry, it was " ++ show randNumber
        -- Do it all again!
        askForNumber newGen
