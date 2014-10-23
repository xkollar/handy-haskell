module Main ( main ) where

import Control.Arrow
import Data.Function
import Data.List
import Data.Maybe
import System.Environment ( getArgs )
import System.Time ( formatCalendarTime, getClockTime, toUTCTime )
import Text.Printf


-- import qualified Data.Map as M
-- import qualified Data.Set as S

{-
 - Simple tool to help with evaluating minesweeper grids
 - for 2014-10 JS puzzle.
 -}

{-

-- example :: [(Point,Int)]
-- example = [((0,2),4), ((1,0),4), ((3,0),2), ((3,2),4)]

      0   1   2   3
    .---.---.---.---.
 0  |   | 4 |   | 2 |
    |---|---|---|---|
 1  |   | S |   |   |
    |---|---|---|---|
 2  | 4 |   |   | 4 |
    |---|---|---|---|
 3  |   |   |   |   |
    '---'---'---'---'

-}

type Point = (Int,Int)
data FieldContent = M | E deriving (Eq, Show)

hRange, vRange :: (Int, Int)
hRange = (0,3)
vRange = (0,3)

expandRage :: (Int, Int) -> [Int]
expandRage = uncurry enumFromTo

between :: Int -> Int -> Int -> Bool
between l h x = l <= x && x <= h

isInside :: Point -> Bool
isInside (x,y) = uncurry between hRange x && uncurry between vRange y

surroundings :: Point -> [Point]
surroundings (x,y) = [(x+dx,y+dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]

-- let det = map fst example in (L.\\ det) . L.nub . filter isInside $ concatMap surroundings det

mineValuations :: [a] -> [[(a, FieldContent)]]
mineValuations s = f [] s [] where
    f a [] r = a : r
    f a (x:xs) r = f ((x,E):a) xs (f ((x,M):a) xs r)

-- let det = map fst example in length . mineValuations . (L.\\ det) . L.nub . filter isInside $ concatMap surroundings det

look :: [(Point, FieldContent)] -> Point -> FieldContent
look s x = fromMaybe E $ lookup x s

-- let det = map fst example in filter (\ fill -> all (\(p,n) -> (n==) . length . filter (M==) . map (look fill) $ surroundings p) example) . mineValuations . (\\ det) . nub . filter isInside $ concatMap surroundings det

-- let det = map fst example in mapM_ print . zip [1..] . filter (\ fill -> all (\(p,n) -> look fill p == E && ((n==) . length . filter (M==) . map (look fill) $ surroundings p)) example) . mineValuations . (\\ det) . nub . filter isInside $ concatMap surroundings det

-- let det = map fst example in mapM_ print . sort . map (\ x -> (fst (head x), (length *** length) $ partition ((E==) . snd) x)) . foldr (zipWith $ (:)) (repeat []) . filter (\ fill -> all (\(p,n) -> look fill p == E && ((n==) . length . filter (M==) . map (look fill) $ surroundings p)) example) . mineValuations . (\\ det) . nub . filter isInside $ concatMap surroundings det

genPossibilities :: [(Point, Int)] -> [[(Point, FieldContent)]]
genPossibilities mines = filter (\ fill -> all (\(p,n) -> look fill p == E && ((n==) . length . filter (M==) . map (look fill) $ surroundings p)) mines)
    . mineValuations
    . (\\ det)
    . nub
    . filter isInside
    $ concatMap surroundings det where
        det = map fst mines

compute :: [(Point, Int)] -> [(Point, (Int, Int))]
compute = sort
    . map (\ x -> (fst (head x), (length *** length) $ partition ((E==) . snd) x))
    . transpose
    . genPossibilities

showGrid :: [[String]] -> IO ()
showGrid info = putStr . unlines . map (intercalate "|") $ info' where
    info' = map (map (\ x -> take w $ ' ' : x ++ repeat ' ')) info
    w = 2 + maximum (concatMap (map length) info)

mkGrid :: Show a => [[a]] -> [[String]]
mkGrid s = info where
    info = map (map show) s

maximum' :: Ord a => [a] -> Maybe a
maximum' [] = Nothing
maximum' s = Just $ maximum s

bestRatio :: [(Point, Int)] -> Maybe Rational
bestRatio mines = maximum'
    . filter (1 /=)
    . map ((\ (e,m) -> toRational m / toRational (m+e)) . snd . (\ x -> (fst (head x), (length *** length) $ partition ((E==) . snd) x)))
    . transpose
    $ genPossibilities mines

magic :: [(Point, Int)] -> IO ()
magic mines = showGrid gridPoints where
    x = compute mines
    disp = maybe "-" (\ (e,m) -> let rat = toRational m / toRational (m+e) in show rat ++ " (" ++ printf "%.2f" (100 * fromRational rat :: Double) ++ ")")
    gridPoints = [ [ disp $ lookup (a,b) x | a <- expandRage hRange ] | b <- expandRage vRange ]

-- based only on borders, not other mines...
neighbourCount :: Point -> Int
neighbourCount = length . filter isInside . surroundings

freeNeighbourCount :: [Point] -> Point -> Int
freeNeighbourCount m x = length . filter (flip notElem m) . filter isInside $ surroundings x

allPoints :: [(Int,Int)]
allPoints = [ (x,y) | x <- expandRage hRange, y <- expandRage vRange ]

nCombinations :: Int -> [a] -> [[a]]
nCombinations 0 _ = [[]]
nCombinations n s = [ x:xs | x:t <- tails s , xs <- nCombinations (n-1) t ]

-- Symmetries for square 4x4, (0,0) -> (3,3)
pointSymmetries :: Point -> [Point]
pointSymmetries p = [p, r1, r2, r3, vflip p, hflip p, mflop p, aflop p ] where
    r1 = rot p
    r2 = rot r1
    r3 = rot r2

    rot :: Point -> Point
    rot (x,y) = (y, 3-x)

    vflip :: Point -> Point
    vflip (x,y) = (3-x,y)

    hflip :: Point -> Point
    hflip (x,y) = (x,3-y)

    mflop :: Point -> Point
    mflop (x,y) = (y,x)

    aflop :: Point -> Point
    aflop (x,y) = (3-y,3-x)

fcSymmetries :: [Point] -> [[Point]]
fcSymmetries = transpose . map pointSymmetries

nCombinationsModuloSymmetry :: Int -> [[Point]]
nCombinationsModuloSymmetry n = filter (\ x -> (sort x ==) . minimum . map sort $ fcSymmetries x) $ nCombinations n allPoints

valuations :: [Point] -> [[(Point, Int)]]
valuations t = f t where
    f [] = [[]]
    f (p:s) = do
        c <- [0..(freeNeighbourCount t p - 1) `max` 0]
        r <- f s
        return ((p,c) : r)

valuatedNCombinations :: Int -> [(Maybe Rational, [(Point, Int)])]
valuatedNCombinations = map (bestRatio &&& id) . concatMap valuations . nCombinationsModuloSymmetry

getISO8601Timestamp :: IO String
getISO8601Timestamp = fmap (formatCalendarTime undefined "%Y-%m-%dT%H:%M:%SZ" . toUTCTime) getClockTime

timestampedPrint :: Show a => a -> IO ()
timestampedPrint s = do
    getISO8601Timestamp >>= putStr
    putStr " "
    print s

main' :: [String] -> IO ()
main' ["gen", n] = mapM_ timestampedPrint . increasingBy ((<=) `on` fst) $ valuatedNCombinations (read n)
main' [x] = magic (read x)
main' _ = putStrLn "Read source (sry)"

main :: IO ()
main = getArgs >>= main'

-- [((0,2),4),((1,0),4),((3,0),2),((3,2),4)] -> 16 % 17 (94.12)
-- [((0,2),4),((2,2),7),((1,0),4)] -> 17 % 18 (94.44)
-- [((1,1),7),((3,2),4)] -> 19 % 20 (95.00)

-- | Greedely selects "ordered" subsequence.
increasingBy :: (a -> a -> Bool) -> [a] -> [a]
increasingBy c = g where
    g (x:s) = x : g (dropWhile (not . c x) s)
    g s = s

-- (Just (38 % 39),[((0,1),4),((1,3),4),((2,1),4)])
-- (Just (38 % 39),[((0,1),4),((2,0),4),((2,2),4)])
-- (Just (38 % 39),[((0,2),4),((1,0),4),((2,2),4)])
-- (Just (38 % 39),[((0,2),4),((2,1),4),((2,3),4)])
-- (Just (38 % 39),[((1,0),4),((1,2),4),((3,1),4)])
-- (Just (38 % 39),[((1,1),4),((1,3),4),((3,2),4)])
-- (Just (38 % 39),[((1,1),4),((2,3),4),((3,1),4)])
-- (Just (38 % 39),[((1,2),4),((2,0),4),((3,2),4)])

-- (Just (30 % 31),[((0,0),2),((0,2),2),((2,1),5),((3,2),2)])
-- (Just (30 % 31),[((0,0),2),((0,2),2),((2,3),3),((3,1),2)])
-- (Just (30 % 31),[((0,1),3),((0,2),2),((2,1),3),((3,2),2)])
