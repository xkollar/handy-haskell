import Data.List
import Data.Function
import Control.Arrow

list =
    [ [x1,x2,x3,x4,x5,x6,x7,n]
    | n <- [1..]
    , x1 <- [1..n-7]
    , x2 <- [x1+1..n-6]
    , x3 <- [x2+1..n-5]
    , x4 <- [x3+1..n-4]
    , x5 <- [x4+1..n-3]
    , x6 <- [x5+1..n-2]
    , x7 <- [x6+1..n-1]
    ]

cnt = length . nub . f where
    f [] = []
    f xs@(x:s) = map (x+) (g xs) ++ f s
    g [] = []; g xs@(x:s) = map (x+) xs ++ g s

cumulative p (x:s) = x : f x s where
    f x (y:s) = if p x y then y : f y s else f x s
    f _ []    = []

-- main = putStr . unlines . map (show . (id &&& cnt)) . cumulative ((<) `on` cnt) $ list
-- solution? [1,2,5,14,33,72,125,219]
-- [1,2,5,14,33,72,137,218]
-- [1,2,5,14,33,72,153,218]
-- [1,2,5,14,33,75,128,196]
-- [1,2,5,14,33,75,143,196]
-- [1,2,5,14,33,85,131,185]
-- [1,2,5,40,84,112,130,142]
-- [1,2,5,79,102,114,132,142]
-- [1,3,6,35,75,108,121,130]
-- [1,10,23,56,96,125,128,130]
--
-- Final:
-- ([130,121,108,75,35,6,3,1],120)
-- ([130,128,125,96,56,23,10,1],120)

upperLimit = 130

getMax n = n + n * (n-1) + (n * (n-1) * (n-2)) `div` 6

gm [] = 1
gm [_] = 4
gm [_,_] = 10
gm [_,_,_] = 20
gm [_,_,_,_] = 35
gm [_,_,_,_,_] = 56
gm [_,_,_,_,_,_] = 84
gm [_,_,_,_,_,_,_] = 120

listMax s = [ x | x <- [m..upperLimit], cnt (x:s) == gm s ] where
    m = if null s then 1 else head s + 1

-- dropLast n = reverse . drop n . reverse

list2 =
    [ [x8,x7,x6,x5,x4,x3,x2,x1]
    | x1 <- listMax []
    , x2 <- listMax [x1]
    , x3 <- listMax [x2,x1]
    , x4 <- listMax [x3,x2,x1]
    , x5 <- listMax [x4,x3,x2,x1]
    , x6 <- listMax [x5,x4,x3,x2,x1]
    , x7 <- listMax [x6,x5,x4,x3,x2,x1]
    , x8 <- listMax [x7,x6,x5,x4,x3,x2,x1]
    ]


main = putStr . unlines . map (show . (id &&& cnt)) . cumulative ((>=) `on` head) $ list2
