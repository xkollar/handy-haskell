-- | Functions for maniplulating lists.
module Handy.List where

import Prelude (Bool (..), Ord (..), and, dropWhile, foldr, id, not, otherwise,
    reverse, tail, uncurry, zipWith, (.))

import Control.Applicative ((<*>))

-- | Greedily selects 'ordered' subsequence. Usefull for printing progress of
-- optimization search, for example
--
-- > print . maximum $ possibleSolutions
--
-- can be replaced with more entertaining
--
-- > mapM_ print . increasingBy (<) $ possibleSolutions
increasingBy :: (a -> a -> Bool) -> [a] -> [a]
increasingBy c = g where
    g (x:s) = x : g (dropWhile (not . c x) s)
    g s = s

-- | Test whether element is in ordered list. (Works with infinite lists.)
ordElem :: Ord a => a -> [a] -> Bool
ordElem x (y:s)
    | x > y = ordElem x s
    | x < y = False
    | otherwise = True
ordElem _ [] = False

-- | Produces list of permutations.
-- Preserves @monotonousBy lt s ==> monotonousBy lt (orderedPermutations s)@.
--
-- This function is slower than 'Data.List.permutations', however if you
-- need permurations to be sorted this is more suitable choice as
-- @sort (permutations s)@ not only runs longer than
-- @orderedPermutations (sort s)@ but the latter one is lazy and more memorry
-- efficient.
--
-- prop> sort (permutations s) = orderedPermutations (sort s)
orderedPermutations :: [a] -> [[a]]
orderedPermutations s' = perms [] (reverse s') [] where
    perms :: [a] -> [a] -> [[a]] -> [[a]]
    perms a [] r = a:r
    perms a s r = foldr (\ (x,t) g -> perms (x:a) t . g ) id (selections s) r

    selections :: [a] -> [(a, [a])]
    selections = f id where
        f g (x:s) = (x, g s) : f (g.(x:)) s
        f _ [] = []

-- | Produces list-encoded subsets.
-- Preserves @monotonousBy lt s ==> monotonousBy lt (orderedSubsets s)@.
orderedSubsets :: [a] -> [[a]]
orderedSubsets s' = subs [] s' [] where
    subs :: [a] -> [a] -> [[a]] -> [[a]]
    subs a [] r = reverse a:r
    subs a s r = reverse a :
        foldr (\ (x,t) g -> subs (x:a) t . g ) id (selections s) r

    selections :: [a] -> [(a, [a])]
    selections = f where
        f (x:s) = (x, s) : f s
        f [] = []

-- | Checks monotonicity of list elements by given relation.
isMonotonousBy :: (a -> a -> Bool) -> [a] -> Bool
isMonotonousBy lt = and . (zipWith lt <*> tail)

-- | Check monotonicity by default Ord instance.
isMonotonous :: Ord a => [a] -> Bool
isMonotonous = isMonotonousBy (<)

-- | Break list by delimiters identified by predicate.
-- Variation to @words@/@lines@. Works on infinite lists.
--
-- >>> head . head $ breakAll (const False) [1..]
-- 1
breakAll :: (a -> Bool) -> [a] -> [[a]]
breakAll p = f where
    f = uncurry (:) . g where
        g (x:s) = if p x then ([], f s) else (x:a, b) where
            (a,b) = g s
        g [] = ([], [])
