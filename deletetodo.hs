-- deletetodo from Chapter 09

-- import required modules
import System.IO
import System.Directory
import Data.List

main = do
    -- Open todo.txt and bind it to handle
    handle <- openFile "todo.txt" ReadMode
    -- Open a temp file to write the output to
    (tempName, tempHandle) <- openTempFile "." "temp"
    -- bind the content of todo.txt to contents
    contents <- hGetContents handle
    -- Split it into a list of strings
    let todoTasks = lines contents
        -- Prepend numbers to each string in the list
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    -- Print a header to stdout:
    putStrLn "These are your TO-DO items:"
    -- Make a single, newline delimited string and print it to stdout
    putStr $ unlines numberedTasks
    -- Ask the human which one they would like deleted
    putStrLn "Which one do you want to delete?"
    -- Received the selection
    numberString <- getLine
    -- bind the string to a number
    let number = read numberString
        -- bind todoTask without selected element to newTodoItems
        newTodoItems = delete (todoTasks !! number) todoTasks
    -- Join into a single string before writing
    hPutStr tempHandle $ unlines newTodoItems
    -- Close both files
    hClose handle
    hClose tempHandle
    -- Delete the original file
    removeFile "todo.txt"
    -- Replace the original file with the new file
    renameFile tempName "todo.txt"
