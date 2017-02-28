-- Monoid example from Chapter 11

-- import required modules
import Data.Monoid

-- Write a function that takes two strings, compares their lengths, and
-- returns an Ordering
lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (vowels x `compare` vowels y) `mappend`
                    (x `compare` y)
    where vowels = length . filter (`elem` "aeiou")
