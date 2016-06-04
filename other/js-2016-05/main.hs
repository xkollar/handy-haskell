module Main (main) where

import Control.Arrow
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe

import Handy.List
import Handy.Function

main :: IO ()
main = mapM_ (print . second reverse)
    . increasingBy (<=) . map (annotatel $ product . map snd)
    $ magic inputMod1

input :: [[Char]]
input =
    [ "9_9__9_99"
    , "__5555789"
    , "___4_____"
    , "654333789"
    , "__421____"
    , "__4_2__8_"
    , "6_6666_89"
    , "7_77_778_"
    , "8_88____9"
    ]

readMaybeDigit :: Char -> Maybe Int
readMaybeDigit '0' = Just 0
readMaybeDigit '1' = Just 1
readMaybeDigit '2' = Just 2
readMaybeDigit '3' = Just 3
readMaybeDigit '4' = Just 4
readMaybeDigit '5' = Just 5
readMaybeDigit '6' = Just 6
readMaybeDigit '7' = Just 7
readMaybeDigit '8' = Just 8
readMaybeDigit '9' = Just 9
readMaybeDigit _ = Nothing

-- unMaybe :: (a -> Maybe b) -> [a] -> [b]

liftMaybe (a,Just b) = Just (a,b)
liftMaybe _ = Nothing

removeOrdered :: Ord a => a -> [(a, b)] -> [(a,b)]
removeOrdered x = f where
    f ((v@(y,_)):s)
        | y < x = v : f s
        | x == y = f s
        | otherwise = v:s
    f [] = []

inputMod1 :: [[(Int, Int)]]
inputMod1 = map (f . zip [1..]) $ transpose input where
    f = catMaybes . map (liftMaybe . second readMaybeDigit)

magic :: Ord a => [[(a,b)]] -> [[(a,b)]]
magic = f [] [] where
    f a c [] = a : c
    -- f _ c ([]:_) = c
    f a c ([]:s) = f a c s
    -- f a c (l:s) = foldr (.) id (h : map g l) c where
    f a c (l:s) = foldr (.) id (map g l) c where
        g (p,v) c = f ((p,v):a) c $ map (removeOrdered p) s
        -- h c = f a c s

