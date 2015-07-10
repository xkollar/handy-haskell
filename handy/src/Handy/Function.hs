module Handy.Function
    ( fixEq
    , annotatel
    , annotater
    ) where

import Control.Arrow

-- | Will iterativelly find fixed point of a function (starting from given point) based on 'Eq' instance.
--
-- >>> fixEq cos 0
-- 0.7390851332151607
fixEq :: Eq a => (a -> a) -> a -> a
fixEq f = fun where
    fun x = if x == x' then x else fun x' where
        x' = f x

-- | Annotate value by given function, left version.
--
-- > sort . annotatel read
annotatel :: (a -> b) -> a -> (b, a)
annotatel = (&&& id)

-- | Annotate value by given function, right version.
annotater :: (a -> b) -> a -> (a, b)
annotater = (id &&&)
