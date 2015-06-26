module Handy where

-- | Greedely selects "ordered" subsequence.
increasingBy :: (a -> a -> Bool) -> [a] -> [a]
increasingBy c = g where
    g (x:s) = x : g (dropWhile (not . c x) s)
    g s = s

-- | Test whether element is in ordered list. Works with infinite lists.
ordElem :: Ord a => a -> [a] -> Bool
ordElem x (y:s)
    | x > y = ordElem x s
    | x < y = False
    | otherwise = True
ordElem _ [] = False

-- Preserves `sorted s ==> sorted (orderedPerms s)`
orderedPerms :: [a] -> [[a]]
orderedPerms s = f [] s [] where
    f :: [a] -> [a] -> [[a]] -> [[a]]
    f a [] r = reverse a:r
    f a s r = foldr (\ (x,t) g -> f (x:a) t . g ) id (selections s) r

selections :: [a] -> [(a, [a])]
selections = f id where
    f g (x:s) = (x, g s) : f (g.(x:)) s
    f _ [] = []

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted (x:s) = f x s where
    f v (y:t) = v < y && f y t
    f _ [] = True

