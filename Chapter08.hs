-- Chapter 8 - Making Our Own Types and Typeclasses

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
