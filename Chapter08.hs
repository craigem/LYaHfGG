-- Chapter 8 - Making Our Own Types and Typeclasses

module Shapes
    ( Point(..)
    , Shape(..)
    , surface
    , nudge
    , baseCircle
    , baseRect
    , Person(..)
    ) where

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
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

-- define a car using record syntax
data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)
