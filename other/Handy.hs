module Handy where

import Control.Arrow

fixEq :: Eq a => (a -> a) -> a -> a
fixEq f = fun where
    fun x = if x == x' then x else fun x' where
        x' = f x

-- | Greedely selects "ordered" subsequence.
increasingBy :: (a -> a -> Bool) -> [a] -> [a]
increasingBy c = g where
    g (x:s) = x : g (dropWhile (not . c x) s)
    g s = s

anotater :: (a -> b) -> a -> (a, b)
anotater = (id &&&)

anotatel :: (a -> b) -> a -> (b, a)
anotatel = (&&& id)
