module Scavenger where

import Control.Arrow
import Data.Char ( toUpper )
import Data.Maybe ( fromMaybe )

rot :: Int -> Char -> Char
rot n c = fromMaybe c $ lookup c wholeMap where
    wholeMap = baseMap ++ map toUpperTuple baseMap
    toUpperTuple = toUpper *** toUpper
    baseMap = zip ['a'..'z'] . drop n $ cycle ['a'..'z']
