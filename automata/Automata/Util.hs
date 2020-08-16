module Automata.Util (
    gen, gen', gen3, gen3', map'
) where

gen3 :: (a -> a -> a -> b) -> [a] -> [b]
gen3 f [x, y, z] = [f x y z]
gen3 f (x : xs@(y:z:ys)) = f x y z : gen3 f xs

gen3' :: (a -> a -> a -> b) -> [a] -> [b]
gen3' f [x, y, z] = [f x y z]
gen3' f (x : xs@(y:z:ys)) = let a = f x y z
                            in a `seq` a : gen3' f xs

-- | generate next.
gen :: (a -> a -> b) -> [a] -> [b]
gen _ []     = errorWithoutStackTrace "empty list"
gen f (x:xs) = hGen f x xs

-- | the strict variant of gen.
gen' :: (a -> a -> b) -> [a] -> [b]
gen' _ []     = errorWithoutStackTrace "empty list"
gen' f (x:xs) = hGen' f x xs

hGen :: (a -> a -> b) -> a -> [a] -> [b]
hGen _ _ []     = errorWithoutStackTrace "empty list"
hGen f h [x]    = [h `f` x]
hGen f h (x:xs) = h `f` x : hGen f x xs

hGen' :: (a -> a -> b) -> a -> [a] -> [b]
hGen' _ _ []     = errorWithoutStackTrace "empty list"
hGen' f h [x]    = [h `f` x]
hGen' f h (x:xs) = let y = h `f` x
                   in y `seq` y : hGen' f x xs

-- | strict version map.
map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = let x' = f x
                in x' `seq` x' : map' f xs
