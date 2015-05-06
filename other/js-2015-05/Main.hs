module Main where

import Data.Function (on)
import Data.List
import Data.Set (isSubsetOf, fromList, Set)

type P = (Int,Int)

data Result = R
    { win :: Rational
    , tie :: Rational
    , lose :: Rational
    } deriving Show

grid :: [P]
grid = [ (x,y) | x <- [0..2], y <-[0..2]]

assert :: Bool -> String -> t -> t
assert b t v = if b then v else error t

combine :: [Result] -> Result
combine s = assert (nw + nt + nl == 1) "Total probablility is not 1" $ R nw nt nl  where
    l = fromIntegral $ length s
    nw = sum (map win s) / l
    nt = sum (map tie s) / l
    nl = sum (map lose s) / l

winPositions :: [Set P]
winPositions = fmap fromList
    [ [(0,0),(0,1),(0,2)]
    , [(1,0),(1,1),(1,2)]
    , [(2,0),(2,1),(2,2)]
    , [(0,0),(1,0),(2,0)]
    , [(0,1),(1,1),(2,1)]
    , [(0,2),(1,2),(2,2)]
    , [(0,0),(1,1),(2,2)]
    , [(0,2),(1,1),(2,0)]
    ]

hasWon :: [P] -> Bool
hasWon s = any (`isSubsetOf` s') winPositions where
    s' = fromList s

moves :: [a] -> [(a, [a])]
moves = f id where
    f a (x:s) = (x,a s) : f (a . (x:)) s
    f _ []    = []

anotatel :: (a -> b) -> a -> (b, a)
anotatel f x = (f x, x)

fullWin, fullTie, fullLoose :: Result
fullWin   = R 1 0 0
fullTie   = R 0 1 0
fullLoose = R 0 0 1

valuation :: Result -> Rational
valuation = win

me :: [P] -> [P] -> [P] -> Result
me _m _o [] = fullTie
me m o s =  if any (\(n,_r) -> hasWon (n:m)) ms then fullWin else magic . fmap f $ ms where
    magic = combine . take 1 . sortBy (flip compare `on` valuation)
    ms = moves s
    f (new, rest) = oponent (new:m) o rest

oponent :: [P] -> [P] -> [P] -> Result
oponent _m _o [] = fullTie
oponent m o s =  combine . map f $ ms where
     ms = moves s
     f (new, rest) = if hasWon new' then fullLoose else me m new' rest where
        new' = new:o

meStartWith :: P -> Result
meStartWith p = oponent [p] [] (p `delete` grid)

procOne :: P -> IO ()
procOne s = putStrLn $ show s ++ ": " ++ show (valuation res) ++ ' ':show res where
    res = meStartWith s

main :: IO ()
main = do
    procOne (0,0)
    procOne (0,1)
    procOne (1,1)
    -- others are symmetrical
