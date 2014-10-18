module Main where

import qualified Data.Map as M

import Data.Char
import Data.Function
import Data.List
import Data.Maybe

type Grid = M.Map (Int,Int)

between :: Ord a => a -> a -> a -> Bool
between l u x = l <= x && x <= u

data Direction = Across | Down deriving (Show, Eq, Ord)

type CluePos = (Int, Direction)

data Clue = Clue
    { clue :: String
    , gen :: Grid Char -> [Int]
    }

-- data Clue = Clue
--     { position :: (Int,Int)
--     , clue :: String
--     , direction :: Direction
--     , len :: Int
--     , generator :: Generator -- potentially infinte, ordered
--     } deriving Show

baseCheck :: Int -> Bool
baseCheck n = not hasZero && uniqueNumbers where
    n' = show n
    hasZero = '0' `elem` n'
    uniqueNumbers = n' == nub n'

filterLen :: Integral a => Int -> [a] -> [a]
filterLen n = dropWhile (< (10^(n-1))) . takeWhile (< (10^n))

squareNumber :: Integral a => Int -> [a]
squareNumber n = filterLen n $ map (^2) [1..]

gr =
    [ "___#___#___"
    , "___#___#___"
    , "___#_______"
    , "##______###"
    , "_____#_____"
    , "____###____"
    , "_____#_____"
    , "###______##"
    , "_______#___"
    , "___#___#___"
    , "___#___#___"
    ]

kwak = putStrLn . unlines $ map (concatMap (\x -> [x,'|'])) gr
-- filter baseCheck $ squareNumber 3

sqrtBig :: Integral b => b -> b
sqrtBig n = head . dropWhile ((n<).(^(2::Int))) $ iterate f n where
    f x = (x + n `div` x) `div` 2

primes :: [Int]
primes = 2:3: filter isPrime [5,7..] where

isPrime :: Int -> Bool
isPrime n = f $ takeWhile (<=bound) primes where
    f = notElem 0 . map (mod n)
    bound = sqrtBig n

showMap = unlines

type Pattern = String

match :: Pattern -> String -> Bool
match ('_':s) (_:t) = match s t
match (x:s) (y:t)   = x == y && match s t
match [] []         = True
match _ _           = False

match' p = match p . show
-- test = M.fromList . concat $ zipWith (\ y -> zipWith (\ x n -> ((x,y),n)) [1..]) [1..] gr

-- test2 = map (\((x,y),_) -> maybe '#' id $ M.lookup (x-1,y) test) . sort . filter ((==) ' ' . snd) $ M.toList test

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

baseMapMap :: Grid Char
baseMapMap = M.fromList baseMapList

baseMapList :: [((Int, Int), Char)]
baseMapList = concat $ zipWith (\ y -> zipWith (\ x n -> ((x,y),n)) [1..]) [1..] gr

getPos n = M.lookup n $ M.fromList positions where
    positions = zip [1..] . map fst . filter (\((x,y),_) -> get (x-1,y) == '#' || get (x,y-1) == '#') . sortBy (compare `on` (swap . fst)) . filter (('#' /=) . snd) $ baseMapList where
        get k = M.findWithDefault '#' k baseMapMap

isWall :: Char -> Bool
isWall = (==) '#'

getMap m k = M.findWithDefault '#' k m

getPart (x,y) Across n = (x+n,y)
getPart (x,y) Down   n = (x,y+n)

getPattern m n dir =  takeWhile (not . isWall) $ map (getMap m . getPart (fromJust (getPos n)) dir) [0..]

sqn3 = Clue "A square number" (\ _ -> squareNumber 3)

getNum :: Grid Char -> Int -> Direction -> Maybe Int
getNum m n d = if all isDigit pat then Just $ read pat else Nothing where
    pat = getPattern m n d

clues :: [(CluePos,Clue)]
clues =
    [ (( 1,Across), sqn3)
    , (( 4,Across), sqn3)
    , (( 7,Across), sqn3)
    -- , ((10,Across), Clue "Smaller than 23-down" (\ m -> getNum m 23 Down >>= []))
    , ((11,Across), sqn3)
    , ((12,Across), sqn3)
    ]

-- f 0 _  = [[]]
-- f _ [] = []
-- f k as = [ x : xs | (x:as') <- tails as, xs <- f (k-1) as' ]
--
-- subsequencesOfSize :: Int -> [a] -> [[a]]
-- subsequencesOfSize n xs = let l = length xs
--                           in if n>l then [] else subsequencesBySize xs !! (l-n)
--  where
--    subsequencesBySize [] = [[[]]]
--    subsequencesBySize (x:xs) = let next = subsequencesBySize xs
--                              in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])
