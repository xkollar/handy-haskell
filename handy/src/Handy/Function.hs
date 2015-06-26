module Handy.Function
    ( fixEq
    , anotatel
    , anotater
    ) where

import Control.Arrow

-- | Will find fixed point of a function from fiven starting point based on 'Eq' instance.
--
-- >>> fixEq cos 0
-- 0.7390851332151607
fixEq :: Eq a => (a -> a) -> a -> a
fixEq f = fun where
    fun x = if x == x' then x else fun x' where
        x' = f x

-- | Use funtion to anotate value, left version.
--
-- > sort . anontatel read
anotatel :: (a -> b) -> a -> (b, a)
anotatel = (&&& id)

-- | Use funtion to anotate value, right version.
anotater :: (a -> b) -> a -> (a, b)
anotater = (id &&&)
