import Control.Arrow
import Data.Function
import Data.List
import System.Environment ( getArgs )

import Inputs

checkSol :: [Int] -> Bool
checkSol x = nub (sort x) == sort x && checkSol' x

checkSol' :: [Int] -> Bool
checkSol' s = and $ zipWith check s (tail s)

check :: Int -> Int -> Bool
check x y = (x `mod` y == 0 || y `mod` x == 0) && x /= y

anotater = (id &&&)
anotatel = (&&& id)

increasingBy :: (a -> a -> Bool) -> [a] -> [a]
increasingBy c = g where
    g (x:s) = x : g (dropWhile (not . c x) s)
    g s = s

optsAll :: [a] -> [(a, [a])]
optsAll = f id where
    f _ []    = []
    f a (x:s) = (x,a s) : f (a . (x:)) s

-- relevantPrimes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
relevantPrimes :: [Int]
relevantPrimes = takeWhile (<100) $ sieve [2..] where
    sieve (x:s) = x : sieve (filter ((/=) 0 . flip mod x) s)

relevantFactors = f relevantPrimes where
    f _ 1 = []
    f (p:s) n = if m == 0 then p : f (p:s) d else f s n where
        (d,m) = n `divMod` p

magic  :: (t -> (Int, Int, [Int]) -> [(Int, t)]) -> t -> (Int, Int, [Int]) -> [[Int]] -> [[Int]]
magic f = solve where
        solve as (x,y,s) rest
            | y > x     = solve as (y,x,reverse s) rest
            | otherwise = case f as (x,y,s) of
                [] -> s:rest
                t -> foldr (.) id (map (\ (x',as') -> solve as' (x',y,x':s)) t) rest

magicOpts :: [Int] -> (Int, Int, [Int]) -> [(Int, [Int])]
magicOpts s (x,_,_) = sortBy (compare `on` anotatel (length . relevantFactors . fst))
    . filter (check x . fst)
    $ optsAll s

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> solPrinter
            . increasingBy ((<=) `on` length)
            $ magic magicOpts (inputs \\ [96])  (96,96,[96]) []
        _ -> putStrLn "read source...."

target :: Int
target = 84 - 2 -- 51, 57, 69 ... choose one
