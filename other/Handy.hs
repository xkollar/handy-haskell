module Handy where

import Control.Arrow
import Data.List ( groupBy )

fixEq :: Eq a => (a -> a) -> a -> a
fixEq f = fun where
    fun x = if x == x' then x else fun x' where
        x' = f x

-- | Greedely selects "ordered" subsequence.
increasingBy :: (a -> a -> Bool) -> [a] -> [a]
increasingBy c = map head . groupBy ((not .) . c)

-- | Test whether element is in ordered list. Works with infinite lists.
ordElem :: Ord a => a -> [a] -> Bool
ordElem x (y:s)
    | x > y = ordElem x s
    | x < y = False
    | otherwise = True
ordElem _ [] = False

anotater :: (a -> b) -> a -> (a, b)
anotater = (id &&&)

anotatel :: (a -> b) -> a -> (b, a)
anotatel = (&&& id)