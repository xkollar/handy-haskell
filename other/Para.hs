module Para where

import Data.List

import System.Environment ( getArgs )

mainWrapper :: ([String] -> IO ()) -> IO ()
mainWrapper main = do
    getArgs >>= main


-- eliminateSymmetries :: Symmetric a => [a] -> [a]


type Coordinate = (Int,Int)
type Map = [Coordinate]

hRange = (0,3)
vRange = (0,3)

allPoints :: [(Int,Int)]
allPoints = [ (x,y) | x <- f hRange, y <- f vRange ] where
    f = uncurry enumFromTo

nCombinations :: Int -> [a] -> [[a]]
nCombinations 0 s = [[]]
nCombinations n s = [ x:xs | x:t <- tails s , xs <- nCombinations (n-1) t ]

coordinateSymmetries :: Coordinate -> [Coordinate]
coordinateSymmetries p = [p, r1, r2, r3, vflip p, hflip p, mflop p, aflop p ] where
    r1 = rot p
    r2 = rot r1
    r3 = rot r2

coordinateCanonicalize :: [Coordinate] -> [Coordinate]
coordinateCanonicalize = sort

mapSymmetries :: Map -> [Map]
mapSymmetries [] = []
mapSymmetries s = foldr (zipWith (:)) (repeat []) $ map coordinateSymmetries s

mapCanonicalize :: [Map] -> [Map]
mapCanonicalize = sort . map coordinateCanonicalize

-- rotate around (1.5,1.5)
rot :: Coordinate -> Coordinate
rot (x,y) = (y, 3-x)

vflip :: Coordinate -> Coordinate
vflip (x,y) = (3-x, y)

hflip :: Coordinate -> Coordinate
hflip (x,y) = (x, 3-y)

mflop :: Coordinate -> Coordinate
mflop (x,y) = (y, x)

aflop :: Coordinate -> Coordinate
aflop (x,y) = (3-y, 3-x)

nCombinationsModuloSymmetry n = filter (\ x -> (sort x ==) . minimum . map sort $ mapSymmetries x) $ nCombinations n allPoints

neighbourCount :: Coordinate -> Int
neighbourCount p = 2

valuations :: [Coordinate] -> [[(Coordinate, Int)]]
valuations [] = [[]]
valuations (p:s) = do
    c <- [0..neighbourCount p - 1]
    r <- valuations s
    return ((p,c) : r)

