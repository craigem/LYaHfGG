doubleMe x = x + x
doubleUS x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
						then x
						else x*2
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
conanO'Brien = "It's a-me, Conan O'Brien!"
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
-- Replaced later
--length' xs = sum [1 | _ <- xs]
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z
-- Replaced later
--factorial :: Integer -> Integer
--factorial n = product [1..n]
circumference :: Float -> Float
circumference r = 2 * pi * r
circumference' :: Double -> Double
circumference' r = 2 * pi * r

-- Sytnax in Functions
-- Pattern matching
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"
sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)
charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
charName x = "No character name found!"
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
first :: (a, b, c) -> a
first (x, _, _) = x
second :: (a, b, c) -> b
second (_, y, _) = y
third :: (a, b, c) -> c
third (_, _, z) = z
{- re-written below
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x
-}
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y
-- Re-write length using pattern matching
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs
-- Re-write sum using patterns
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs
-- Pattern shortcuts
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- Guards, guards!

-- Re-written with where later
-- bmiTell :: (RealFloat a) => a -> a -> String
-- bmiTell weight height
-- 	| weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
-- 	| weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
-- 	| weight / height ^ 2 <= 30.0 = "You're fat! Lose some weigh, fatty!"
-- 	| otherwise   = "You're a whale, congratulations!"

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
	| a > b		= GT
	| a == b	= EQ
	| otherwise = LT

-- Where!?

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
	where 
		(f:_) = firstname
		(l:_) = lastname

{- Rewitten using let below
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2  
-}

-- let it be

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in sideArea + 2 * topArea

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

-- Case expressions

head' :: [a] -> a  
head' xs = case xs of [] -> error "No head for empty lists!"  
                      (x:_) -> x  

describeList :: [a] -> String
describeList xs = "This list is " ++ case xs of [] -> "empty."
                                                [x] -> "a singleton list."
                                                xs -> "a longer list."

-- 5. Recursion

-- Maximum awesome

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- A few more recursive functions

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

