-- Chapter 8 - Making Our Own Types and Typeclasses

module Shapes
    ( Point(..)
    , Shape(..)
    , surface
    , nudge
    , baseCircle
    , baseRect
    , Person(..)
    , Maybe(..)
    ) where

import qualified Data.Map as Map

-- Define Shape
--data Shape = Circle Float Float Float | Rectangle Float Float Float Float

--surface :: Shape -> Float
--surface (Circle _ _ r) = pi * r ^ 2
--surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- Redfine Shape to get the string representation of our values.
-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

-- Make an intermediate data type that defines a point in two-dimensional space
-- which we can use that to make our shapes more understandable.
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- Redfine surface to reflect the above changes.
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- a function that nudges a shape
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (
    Rectangle (
        Point x1 y1) (Point x2 y2)) a b =
    Rectangle (
        Point (x1+a) (y1+b)) (Point (x2+a) (y2+b)
    )

-- make some auxilliary functions that create shapes of some size at the zero
-- coordinates and then nudge those
baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

-- Record Syntax

-- Create a data type that describes a person
-- Stores:
-- first name, surname, age, height, phone number, favorite ice-cream flavor.
--data Person = Person String String Int Float String String deriving (Show)

-- Create a function to get seperate info from a person?
--firstName :: Person -> String
--firstName (Person firstname _ _ _ _ _) = firstname

--lastName  :: Person -> String
--lastName (Person _ lastname _ _ _ _) = lastname

--age :: Person -> Int
--age (Person _ _ age _ _ _) = age

--height :: Person -> Float
--height (Person _ _ _ height _ _) = height

--phoneNumber :: Person -> String
--phoneNumber (Person _ _ _ _ number _) = number

--flavor :: Person -> String
--flavor (Person _ _ _ _ _ flavor) = flavor

-- Re-write the above data type and records more efficiently.
--data Person = Person { firstName :: String
--                     , lastName :: String
--                     , age :: Int
--                     , height :: Float
--                     , phoneNumber :: String
--                     , flavor :: String
--                     } deriving (Show)
-- Re-written below

-- define a car using record syntax
data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)

-- Type parameters

-- Make a function that displays the car's properties in a nice little text
tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) =
        "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

-- Implement a 3D vector type and add some operations for it.
-- Use a parameterized type because even though it will usually contain numeric
-- types, it will still support several of them.

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i * m) (j * m) (k * m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i * l + j * m + k * n

-- Derived instances

data Person = Person
    { firstName :: String
    , lastName :: String
    , age :: Int
    } deriving (Eq, Show, Read)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Type synonyms

phoneBook :: [(String,String)]
phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

type AssocList k v = [(k,v)]


type IntMap = Map.Map Int

-- Use a map from Data.Map to represent the lockers. Map from locker numbers to
-- a pair of whether the locker is in use or not and the locker code.

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
            then Right code
            else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken, "ZD39I"))
    ,(101,(Free, "JAH3I"))
    ,(103,(Free, "IQSA9"))
    ,(105,(Free, "QOTSA"))
    ,(109,(Taken, "893JJ"))
    ,(110,(Taken, "99292"))
    ]

-- Recursive data structures

-- Use algebraic data types to implement our own list
-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

-- define functions to be automatically infix by making them comprised of only
-- special characters:
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

-- Make a function that adds two of our lists together:
infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)


-- a tree is either an empty tree or it's an element that contains some value
-- and two trees. Perfect fit for an algebraic data type.
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- a utility function for making a singleton tree (a tree with just one node)
-- and a function to insert an element into a tree:
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

-- if the element we're looking for is smaller than the root node, check to see
-- if it's in the left sub-tree. If it's bigger, check to see if it's in the right
-- sub-tree:
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

-- Typeclasses 102

-- Define the states of a traffic light.
data TrafficLight = Red | Yellow | Green

-- Implement as an instance of Eq:
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

-- Implement as an instance of Show:
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

-- A yes-no typeclass

-- Even though strictly using Bool for boolean semantics works better in
-- Haskell, let's try and implement that JavaScript-ish behavior anyway. For
-- fun!  Let's start out with a class declaration:
class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

-- Make a function that mimics the if statement, but it works with YesNo
-- values:
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

-- The Functor typeclass

-- Look at fmap as if it were a function made only for Tree:
instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

-- Kinds and some type-foo

class Tofu t where
    tofu :: j a -> t a j

data Frank a b = Frank {frankField :: b a} deriving (Show)

instance Tofu Frank where
    tofu x = Frank x

data Barry t k p = Barry { yabba :: p, dabba :: t k}

instance Functor (Barry a b) where
    fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba =y}
