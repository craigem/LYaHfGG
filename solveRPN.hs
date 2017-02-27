-- Reverse Polish Notation from Chapter 10

-- import required modules
import Data.List

-- Resolve a set of argumatents in reverse polish notation
solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*" = (x * y):ys
            foldingFunction (x:y:ys) "+" = (x + y):ys
            foldingFunction (x:y:ys) "-" = (x - y):ys
            foldingFunction (x:y:ys) "/" = (x / y):ys
            foldingFunction (x:y:ys) "^" = (x ** y):ys
            foldingFunction (x:xs) "ln" = log x:xs
            foldingFunction xs "sum" = [sum xs]
            foldingFunction xs numberString = read numberString:xs
