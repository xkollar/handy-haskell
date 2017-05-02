module Main (main) where

import Control.Arrow
import Data.List

f :: [Int] -> [[Int]] -> [[Int]]
f _ [] = []
f [] rem = [[]]
f (x:s) rem = concatMap (concatMap (\ (y,t) -> map (y:) $ f s [t]) . g x) rem where

g :: Int -> [Int] -> [(Int,[Int])]
g _ [] = []
g n (y:t) = if other `elem` t then (y,other `delete` t) : rec else rec where
    rec = map (second (y:)) $ g n t
    other = y + n + 1

upperLimmit :: Int -> Int
upperLimmit n = 2*n+1

solveFor :: Int -> [[(Int, Int)]]
solveFor n = map (reverse . zipWith (\m x -> (x,x+m+1)) lst) $ f lst [[1..upperLimmit n]] where
    lst = reverse [1..n]

update :: a -> (Int, Int) -> [a] -> [a]
update v (a,b) = zipWith (\n c -> if n == a || n == b then v else c) [1..]

showFor :: Int -> [(Int, Int)] -> [Char]
showFor n = flip id (replicate (upperLimmit n) '-') . foldr (.) id . zipWith update ['A'..]

allFor :: Int -> [[Char]]
allFor n = map (showFor n) $ solveFor n

main :: IO ()
main = mapM_ putStrLn $ allFor 26
