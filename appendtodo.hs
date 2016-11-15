-- appendtdo from Chapter 09

-- import required modules

import System.IO

main = do
    todoItem <- getLine
    appendFile "todo.txt" (todoItem ++ "\n")
