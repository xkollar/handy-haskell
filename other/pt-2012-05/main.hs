import Control.Arrow
import Data.List

ex1 :: [[Int]]
ex1 =
    [ [1,3,5,4,2,6]
    , [2,1,4,3,6,5]
    , [5,6,3,4,1,2]
    , [6,2,4,5,3,1]
    ]

exV10S6 :: [[Int]]
exV10S6 =
    [ [1,2,3,5,4,6,7,8,9,10]
    , [2,3,4,7,9,1,5,10,6,8]
    , [4,8,5,1,2,3,10,9,6,7]
    , [7,6,9,10,3,2,1,5,8,4]
    , [8,6,10,5,1,9,7,4,3,2]
    , [10,9,8,7,6,4,5,3,2,1]
    ]

check :: [[Int]] -> Bool
check = and . zipWith (==) [1..] . sort . concat . map (tail . init . scanl (+) 0)

show1 x = (concatMap ((++ "*") . flip replicate '-' . (-1+)) $ x) ++ " " ++ show x

showAll = unlines . map show1

nice = putStr . showAll

helper x = x ++ reverse (map reverse x)

show2 s t = map (\ x -> if x `elem` t then '*' else '-') s

helper' n x = x ++ reverse (map (map ((1+n)-)) x)

-- let f n = (n * (n+1) `div` 2 - 1) `divMod` (n-1) in putStr . unlines . map show . map (id *** fst) . filter ((0==) . snd . snd) $ map (id &&& f) [2..20]
-- putStr . unlines . map (showAll . helper) . nub . map sort $ filter (check . helper) [ [a,b] | let p = permutations, a <- p [1..10], b <- p (reverse [1..10]), c <- p [2,6,10,5,7,3,4,9,1,8]]
-- writeFile "syms-for-6-4.txt" . unlines . map (showAll . helper) . nub . sort . map sort $ filter (check . helper) [ [a,b] | let lst = permutations [1..6], a <- lst, b <- lst]

main = getContents >>= print . strcheck where
    strcheck = and . zipWith (==) [1..] . sort . concat . map (tail . init . scanl (+) 0) . map (map read . words) . lines
