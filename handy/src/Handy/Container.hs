{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module:       $HEADER$
-- Description:  Unified interface for container-like data structures.
--
-- Unified interface for container-like data structures.
module Handy.Container
    (
    -- * Class
      Container(..)
    -- * Data structures and instances
    , MaxSet
    , MinSet
    , Queue
    -- * Utility functions
    , fromListL
    , fromListR
    , toList
    ) where

import Data.Coerce
import qualified Data.List as List
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as Set hiding (Set)

-- | Class for containers @c@ of elements of type @a@. It is a
-- multi-parametric type class as some data structures
-- have constraints on what kind of elements they can contain.
class Container c a where
    empty :: c a
    -- ^ Empty container.
    insert :: a -> c a -> c a
    -- ^ Insert element into container.
    get :: c a -> Maybe (a, c a)
    -- ^ Retrieve element from container.
    null :: c a -> Bool
    -- ^ Emptyness discriminator.
    null = isNothing . get

-- | Stack-like instance for list.
instance Container [] a where
    empty = []
    insert = (:)
    get = \case
        [] -> Nothing
        (x:s) -> Just (x,s)
    null = List.null

-- | Newtype wrapper around Data.Set that 'get's you minimal element.
-- Converting to and from 'Set' can be done via 'coerce'.
newtype MinSet a = MinSet (Set a)
    deriving (Eq, Show)

-- | Min-heap-like instance for Set.
instance Ord a => Container MinSet a where
    empty = MinSet Set.empty
    insert x (MinSet s) = MinSet (Set.insert x s)
    get (MinSet s) = coerce $ Set.minView s
    null (MinSet s) = Set.null s

-- | Newtype wrapper around Data.Set that 'get's you maximal element.
-- Converting to and from 'Set' can be done via 'coerce'.
newtype MaxSet a = MaxSet (Set a)
    deriving (Eq, Show)

-- | Max-heap-like instance for Set.
instance Ord a => Container MaxSet a where
    empty = MaxSet Set.empty
    insert x (MaxSet s) = MaxSet (Set.insert x s)
    get (MaxSet s) = coerce $ Set.maxView s
    null (MaxSet s) = Set.null s

-- | Simple Queue with possibility to make insert and get in (amortized)
-- constant time.
data Queue a = Queue [a] [a]

instance Container Queue a where
    empty = Queue [] []
    insert x (Queue i o) = Queue (x:i) o
    get (Queue i o) = case o of
        x:s -> Just (x, Queue i s)
        [] -> case reverse i of
            x:s -> Just (x, Queue [] s)
            [] -> Nothing
    null (Queue [] []) = True
    null _ = False

-- | Insert elements into empty data structure, one by one from left.
fromListL :: Container c a => [a] -> c a
fromListL = List.foldl' (flip insert) empty

-- | Insert elements into empty data structure, one by one from right.
fromListR :: Container c a => [a] -> c a
fromListR = foldr insert empty

-- | Create list from elements of data strcucure, in order they would
-- be returned by get.
toList :: Container c a => c a -> [a]
toList = List.unfoldr get

-- kindaHeapSort :: Ord a => [a] -> [a]
-- kindaHeapSort = toList . from where
--     from :: Ord a => [a] -> MinSet a
--     from = fromListL
