module Handy.Function where

import Control.Arrow

fixEq :: Eq a => (a -> a) -> a -> a
fixEq f = fun where
    fun x = if x == x' then x else fun x' where
        x' = f x

anotatel :: (a -> b) -> a -> (b, a)
anotatel = (&&& id)

anotater :: (a -> b) -> a -> (a, b)
anotater = (id &&&)
