-- Applicatives example from Chapter 11

-- import required modules

--  use the applicative style to zip together an arbitrary amount of lists with
-- a function
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

-- Implement a function that takes a list of applicatives and returns an
-- applicative that has a list as its result value
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])
