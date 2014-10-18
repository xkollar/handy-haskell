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

opts :: Int -> [Int] -> [Int] -> [(Int, [Int])]
opts _ [] s = aritySort $ optsAll s
opts n (x:_) s = r where
    r = aritySort
        . filter (\ y -> n' <= 0 || (not . null $ snd y))
        . map (id *** (concat . filter ((<=) n' . length) . split))
        . filter (check x . fst)
        $ optsAll s
    n' = n - 1

optsSimple :: Int -> [Int] -> [Int] -> [(Int, [Int])]
optsSimple _ [] s = optsAll s
optsSimple n (x:_) s = r where
    r = filter (\ y -> n' <= 0 || (not . null $ snd y))
        . map (id *** (concat . filter ((<=) n' . length) . split))
        . filter (check x . fst)
        $ optsAll s
    n' = n - 1

reveseOpts :: Int -> [Int] -> [Int] -> [(Int, [Int])]
reveseOpts n s = reverse . opts n s

-- sort by arity
-- aritySort :: [Int] -> [(c, [Int])] -> [(c, [Int])]
-- aritySort s = map (id *** sortBy (compare `on` (\ x -> length $ filter (check x) s)))
aritySort :: [(Int, [Int])] -> [(Int, [Int])]
aritySort = sortBy (compare `on` uncurry (flip arity))

neighbours :: [Int] -> Int -> [Int]
neighbours s = flip filter s . check

-- how many of first agument neighbours with second argument
arity :: [Int] -> Int -> Int
arity s = length . neighbours s

fixEq :: Eq a => (a -> a) -> a -> a
fixEq f = fun where
    fun x = if x == x' then x else fun x' where
        x' = f x

split [] = []
split (x:s) = (x:v) : split r where
    (t,u) = partition (check x) s
    (v,r) = f t u
    f [] s = ([], s)
    f s [] = (s,[])
    f (x:s) t = (x:c, d) where
         (a,b) = partition (check x) t
         (c,d) = f (a++s) b

baseSolver :: (Int -> [a] -> [a] -> [(a,[a])])
           -- ^ Enumerates possible options...
           -> Int
           -- ^ remaining depth
           -> [a]
           -- ^ walked nodes (last on top)
           -> [a]
           -- ^ available nodes
           -> [[a]]
           -- ^ rest (for optimization, to avoid (++))
           -> [[a]]
baseSolver f = solve where
    solve n a s rest = case id $! f n a s of
        [] -> a:rest
        t -> foldr (.) id (map (\ (x,u) -> solve (n-1) (x:a) u) t) rest

startingWith :: (Int -> [Int] -> [Int] -> [(Int, [Int])]) -> Int -> [Int] -> [Int] -> [[Int]]
startingWith o n i s = baseSolver o (n - length s) (reverse s) (i \\ s) []

solPrinter :: [[Int]] -> IO ()
solPrinter = mapM_ (print . anotatel length)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> solPrinter
            . increasingBy ((<=) `on` length)
            $ baseSolver opts target [] inputs []
        ["ultraprunedmagic"] ->
            solPrinter . increasingBy ((<=) `on` length) $ baseSolver opts 56 [] nodes [] where
                nodes = ((\ s -> filter (not . canBeRemoved s) s) inputs) \\ [74,37]
                bnd = 77 - length nodes
        [s] -> solPrinter
            . increasingBy ((<=) `on` length)
            $ startingWith opts target inputs (read s)
        ["reverse",s] -> solPrinter
            . increasingBy ((<=) `on` length)
            $ startingWith reveseOpts target inputs (read s)
        [n,s] -> solPrinter
            . increasingBy ((<=) `on` length)
            $ startingWith opts (read n) inputs (read s)
        ["reverse",n,s] -> solPrinter
            . increasingBy ((<=) `on` length)
            $ startingWith reveseOpts (read n) inputs (read s)
        ["use",n,s] -> solPrinter
            . increasingBy ((<=) `on` length)
            $ baseSolver opts (read n) [] (read s) []
        _ -> putStrLn "read source...."

target :: Int
target = 84 - 2 -- 51, 57, 69 ... choose one

-- canBeRemoved s n = null chs where
canBeRemoved s n = length (neighbours s n) > 3 && null chs && any (\ m -> (<=1) . length . filter (/=n) . filter (m<) $ neighbours s m) ps where
    chs = filter (n<) $ neighbours s n
    ps = hasse_diagram_parents s n
    -- any (\ n -> (<1) . length . filter (/=100) . filter (n<) $ neighbours inputs n) [20,50]

canBeRemoved' s n = (chs,map (\ m -> filter (/=n) . filter (m<) $ neighbours s m) ps) where
    chs = filter (n<) $ neighbours s n
    ps = hasse_diagram_parents s n

hasse_diagram_parents s n = f . reverse . sort . filter (check n) $ filter (<n) s where
    f (x:t) = x : f (filter ((/= 0) . mod x) t)
    f [] = []
