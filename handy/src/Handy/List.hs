-- | Functions for maniplulating lists.
module Handy.List where

-- | Greedily selects 'ordered' subsequence. Usefull for printing progress of optimization search,
-- for example 'print . maximum $ possibleSolutions' can be replaced with more entertaining
-- 'mapM_ print . increasingBy (<) $ possibleSolutions'.
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
-- Preserves 'monotonousBy lt s ==> monotonousBy lt (orderedPermutations s)'.
orderedPermutations :: [a] -> [[a]]
orderedPermutations s' = perms [] s' [] where
    perms :: [a] -> [a] -> [[a]] -> [[a]]
    perms a [] r = reverse a:r
    perms a s r = foldr (\ (x,t) g -> perms (x:a) t . g ) id (selections s) r

    selections :: [a] -> [(a, [a])]
    selections = f id where
        f g (x:s) = (x, g s) : f (g.(x:)) s
        f _ [] = []

-- | Produces list-encoded subsets.
-- Preserves 'monotonousBy lt s ==> monotonousBy lt (orderedSubsets s)'.
orderedSubsets :: [a] -> [[a]]
orderedSubsets s' = subs [] s' [] where
    subs :: [a] -> [a] -> [[a]] -> [[a]]
    subs a [] r = reverse a:r
    subs a s r = reverse a : foldr (\ (x,t) g -> subs (x:a) t . g ) id (selections s) r

    selections :: [a] -> [(a, [a])]
    selections = f where
        f (x:s) = (x, s) : f s
        f [] = []

-- | Checks monotonicity of list elements by given relation.
isMonotonousBy :: (a -> a -> Bool) -> [a] -> Bool
isMonotonousBy _ [] = True
isMonotonousBy lt (x:s) = f x s where
    f v (y:t) = v `lt` y && f y t
    f _ [] = True

-- | Check monotonicity by default Ord instance.
isMonotonous :: Ord a => [a] -> Bool
isMonotonous = isMonotonousBy (<)
