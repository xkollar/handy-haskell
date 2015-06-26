-- Choose an n -omino (call it T) and place as many copies of it as you can on the 10-by-10 board below.
-- • An n -omino is a connected region of n cells. You get to choose n .
-- • Rotations and reflections of T are allowed.
-- • Copies of T may not overlap with each other.
-- • Any given copy of T may not be placed over two cells of the same value.
-- • The score for each copy of T is the product of the cells it covers.
-- • Your total score is the sum of the scores for your T ‘s.
--
-- What’s the highest total score you can get?

module Main where

import Control.Arrow
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.Set hiding (map, filter)
import qualified Data.Set as S
import System.Environment ( getArgs )

type Point = (Int,Int)

type Shape = Set Point

input :: [[Int]]
input =
    [[6,7,5,2,6,4,2,1,4,4]
    ,[5,4,6,7,7,2,4,2,6,1]
    ,[5,1,7,3,4,5,5,4,7,4]
    ,[2,7,6,5,1,1,2,4,6,1]
    ,[1,5,3,5,3,5,3,4,5,1]
    ,[6,1,2,3,4,4,5,7,2,3]
    ,[1,6,5,3,3,6,5,1,1,7]
    ,[2,1,1,2,1,7,1,3,3,3]
    ,[7,4,4,6,3,4,1,1,6,2]
    ,[4,6,5,6,2,3,7,2,3,6]
    ]

inputMap :: M.Map Point Int
inputMap = M.fromList . concat $ zipWith (\ y r -> zipWith (\ x v -> ((x,y),v)) rowindex r) colindex input where
    rowindex = iterate (+1) 0
    colindex = iterate (+1) 0

--   [][]
-- []  []
-- [][][]
exampleShape :: Shape
exampleShape = fromList [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1),(2,2)]

showShape :: Shape -> String
showShape s = unlines [concatMap (\b -> if b then "[]" else "  ") [(x,y) `S.member` s | x <- g bx] | y <- g by] where
    g = uncurry enumFromTo
    (bx,by) = dimenstions s

dimenstions :: Shape -> ((Int,Int),(Int,Int))
dimenstions = (f *** f) . unzip . toList where
    f = minimum &&& maximum

printShape :: Shape -> IO ()
printShape = putStr . showShape

printShapes :: [Shape] -> IO ()
printShapes = mapM_ (\ s -> print s >> printShape s >> putStrLn "-------------------" )

tup :: (a -> b -> c) -> (a,a) -> (b,b) -> (c,c)
tup f (x1,y1) (x2,y2) = (f x1 x2,f y1 y2)

translate :: (Int,Int) -> Shape -> Shape
translate d = S.map (tup (+) d)

neighbours :: Point -> [Point]
neighbours t = zipWith (tup (+)) [(-1,0),(0,-1),(0,1),(1,0)] $ repeat t

mins :: [Point] -> (Int,Int)
mins = (minimum *** minimum) . unzip

align :: Shape -> Shape
align ts = translate d ts where
    d = (negate *** negate) . mins $ toList ts

rotateP90 :: Point -> Point
rotateP90 (x,y) = (-y,x)

rotateS90 :: Shape -> Shape
rotateS90 = S.map rotateP90

flipP :: Point -> Point
flipP (x,y) = (-x,y)

flipS :: Shape -> Shape
flipS = S.map flipP

variants :: Shape -> [Shape]
variants s = f s ++ f (flipS s) where
    f = take 4 . iterate rotateS90

uniqeVariants :: Shape -> [Shape]
uniqeVariants = nub . map align . variants

norm :: Shape -> Shape
norm = minimum . sort . map align . variants

gen :: Shape -> [Shape]
gen s = nub . filter ((n==) . size) . fmap (norm . flip S.insert s) . nub . concatMap neighbours $ toList s where
    n = size s + 1

example_sol_7_15120 :: [Shape]
example_sol_7_15120 =
    [fromList [(2,2),(2,3),(3,2),(4,2),(5,2),(5,3),(6,3)]
    ,fromList [(4,6),(5,5),(5,6),(6,5),(7,5),(8,5),(8,6)]
    ,fromList [(1,1),(1,2),(1,3),(1,4),(2,1),(2,4),(2,5)]
    ]

example_sol_7_20160 :: [Shape]
example_sol_7_20160 =
    [fromList [(5,3),(5,4),(5,5),(5,6),(5,7),(6,3),(6,4)]
    ,fromList [(7,4),(7,5),(8,4),(8,5),(8,6),(8,7),(8,8)]
    ,fromList [(0,3),(1,3),(2,3),(3,2),(3,3),(4,2),(4,3)]
    ,fromList [(2,9),(3,9),(4,9),(5,8),(5,9),(6,8),(6,9)]
    ]

example_sol_6_19200 :: [Shape]
example_sol_6_19200 = [fromList [(1,9),(2,7),(2,8),(2,9),(3,6),(3,7)]
    ,fromList [(0,8),(1,6),(1,7),(1,8),(2,5),(2,6)]
    ,fromList [(6,2),(6,3),(7,2),(8,1),(8,2),(9,1)]
    ,fromList [(2,1),(3,1),(3,2),(4,2),(5,2),(5,3)]
    ,fromList [(1,1),(1,2),(2,2),(2,3),(2,4),(3,4)]
    ,fromList [(4,8),(4,9),(5,6),(5,7),(5,8),(6,6)]
    ,fromList [(7,3),(8,3),(8,4),(8,5),(9,5),(9,6)]
    ]

current_sol :: [Shape]
current_sol = example_sol_7_20160
-- current_sol = example_sol_6_19200

current_max :: Int
current_max = fromJust $ valuate current_sol inputMap

-- (1,381)
-- (2,894)
-- (3,2982)
-- (4,10368)
-- (5,29880)
-- (6,56880)
-- (7,55440)
-- When we find solution whose valuation is M, then we know that we do not need to search through
-- n-ominos for those n than `upperBounds s input` < M
--
-- >>> sum . map (sum . map product) $ map (findopt 7) $ Data.List.foldr (zipWith (:)) (repeat []) input
-- 5040
--
-- so we do not have to consider 1,2,3
--
-- >>> valuate
--  [fromList [(2,2),(2,3),(3,2),(4,2),(5,2),(5,3),(6,3)]
--  ,fromList [(4,6),(5,5),(5,6),(6,5),(7,5),(8,5),(8,6)]
--  ,fromList [(1,1),(1,2),(1,3),(1,4),(2,1),(2,4),(2,5)]] inputMap
--Just 15120
--
-- +----------+
-- |          |
-- | 46       |
-- | 17345    |
-- | 76  12   |
-- | 53       |
-- |  2  4572 |
-- |    36  1 |
-- |          |
-- |          |
-- |          |
-- +----------+
-- so we do not have to consider 1,2,3,4

-- >>> valuate
--  [fromList [(5,3),(5,4),(5,5),(5,6),(5,7),(6,3),(6,4)]
--  ,fromList [(7,4),(7,5),(8,4),(8,5),(8,6),(8,7),(8,8)]
--  ,fromList [(0,3),(1,3),(2,3),(3,2),(3,3),(4,2),(4,3)]
--  ,fromList [(2,9),(3,9),(4,9),(5,8),(5,9),(6,8),(6,9)]]
-- Just 20160
--
-- +----------+
-- |          |
-- |          |
-- |   34     |
-- |2765112   |
-- |     5345 |
-- |     4 72 |
-- |     6  1 |
-- |     7  3 |
-- |     41 6 |
-- |  56237   |
-- +----------+
--
-- For 6-ominos
--
-- [fromList [(1,9),(2,7),(2,8),(2,9),(3,6),(3,7)]
-- ,fromList [(0,8),(1,6),(1,7),(1,8),(2,5),(2,6)]
-- ,fromList [(6,2),(6,3),(7,2),(8,1),(8,2),(9,1)]
-- ,fromList [(2,1),(3,1),(3,2),(4,2),(5,2),(5,3)]
-- ,fromList [(1,1),(1,2),(2,2),(2,3),(2,4),(3,4)]
-- ,fromList [(4,8),(4,9),(5,6),(5,7),(5,8),(6,6)]
-- ,fromList [(7,3),(8,3),(8,4),(8,5),(9,5),(9,6)]]
-- +----------+
-- | 433    22|
-- | 44333222 |
-- |  4  3266 |
-- |  44    6 |
-- |  1     66|
-- | 110 55  6|
-- | 100 5    |
-- |110 55    |
-- | 00 5     |
-- +----------+
-- valuation ~> 19200

upperBounds :: [(Int, Int)]
upperBounds = map (id &&& (\ n -> sum . map product $ upperBound n input)) [1..7]

upperBound :: Int -> [[Int]] -> [[Int]]
upperBound n i = f [] id n prepared where
    prepared = reverse . group . sort $ concat i
    f  a  g  0 s  = a : f [] id n (g s)
    f _a _g _m [] = []
    f  a  g  m ([]:s) = f a g m s
    f  a  g  m ((x:s):xs) = f (x:a) (g . (s:)) (m - 1) xs

-- All possible n-ominos for given grid.
-- Limit of 7 because there are only 7 different values and
-- "Any given copy of T may not be placed over two cells of the same value."
-- Grouped by n.
shapes :: [[Shape]]
shapes = take 7 $ iterate (nub . concatMap gen) [initial] where
    initial = fromList [(0,0)] :: Shape

-------------------------------------------------------

magic1 :: Int -> [a] -> [[[a]]]
magic1 n = flip (f id (0::Int)) [] where
    f g m s r
        -- | m >= n       = r
        | length c < n && m /= 0 = r
        | length c < n = g [] : r
        | otherwise    = f (g . (c:)) 0 t (f g (m+1) (tail s) r)
        where
        (c,t) = splitAt n s
    -- f m xs@(x:s) = if m >= n || length xs < n then [[]] else map (i:) (f 0 r) ++ f 1 s where
    --     (i,r) = splitAt n xs
    -- f _ []    = [[]]

findopt :: Int -> [Int] -> [[Int]]
findopt n = maximumBy (compare `on` valuateX) . map (filter isUniq) . magic1 n where
    valuateX x = (sum . map product $ x,x)
    isUniq s = nub s == s

--------------------------------------------


check :: M.Map Point Int -> Shape -> Bool
check i = verify . map (\p -> p `M.lookup` i) . toList where
    verify s = not (any isNothing s) && ok (catMaybes s) where
        ok t = sort (nub t) == sort t

-- possible possitions
-- possible translations for shape on the field
posds :: Shape -> [(Int, Int)]
posds sh = [ (x,y) | x <- [-xa..xa+9-xb], y <- [-ya..ya+9-yb] ] where
    ((xa,xb),(ya,yb)) = dimenstions sh

-- this one does not perform checks (except over the bounds, in which case it returns 0)
valuate' :: [Shape] -> M.Map Point Int -> Int
valuate' s i = sum $ map (valuateShape' i) s where

valuateShape' :: M.Map Point Int -> Shape -> Int
valuateShape' i = product . map (\p -> M.findWithDefault 0 p i) . toList

-- Performs simple check that every independent n-omino is valid (no same number)
valuate :: [Shape] -> M.Map Point Int -> Maybe Int
valuate s i = if all (check i) s then Just (valuate' s i) else Nothing

-- length . filter (not . Data.List.null) . map (concatMap (\ s -> filter (check inputMap) . map (flip translate s) $ posds s) . uniqeVariants) $ shapes !! 6

onNonemptyList :: ([t] -> a) -> [t] -> Maybe a
onNonemptyList _ [] = Nothing
onNonemptyList f s = Just $ f s

-- Data.List.foldr :: (a -> b -> b) -> b -> [a] -> b

nonOverlappingCombinations :: [Shape] -> [[Shape]]
nonOverlappingCombinations s' = magic [] S.empty s'' [] where
    s'' = reverse . map snd . sortBy (compare `on` fst) . map (anotatel (valuateShape' inputMap)) $ s'
    magic a  _ [] r = a : r
    magic a ps s  r = (Data.List.foldr (\ (x,t) f -> g x t f) id $ selections s) r where
        g x t f = (if any (`S.member` ps) $ toList x then magic a ps t else magic (x:a) (ps `S.union` x) t) . f

nonOverlappingCombinations2 :: [Shape] -> [[Shape]]
nonOverlappingCombinations2 s' = magic 0 [] S.empty s'' [] where
    s'' = sortBy (flip (compare `on` fst)) . map (anotatel (valuateShape' inputMap)) $ s'
    magic :: Int -> [Shape] -> Set Point -> [(Int,Shape)] -> [[Shape]] -> [[Shape]]
    magic _n a  _ [] r = a : r
    magic  n a ps s  r
        | n + sum (map fst s) >= current_max = (Data.List.foldr (\ ((m,x),t) f -> g m x t f) id $ selections' s) r
        | otherwise = r
        where
        g m x t f = (if any (`S.member` ps) $ toList x then magic n a ps t else magic (n+m) (x:a) (ps `S.union` x) t) . f

-- list of possible selections with respective remainders
selections :: [a] -> [(a, [a])]
selections = f id where
    f g (x:s) = (x, g s) : f (g.(x:)) s
    f _ [] = []

--- XXX is it sufficient to use this?
selections' :: [a] -> [(a, [a])]
selections' = f where
    f (x:s) = (x, s) : f s
    f [] = []

printShapesOnBoard :: [Shape] -> IO ()
printShapesOnBoard shs = putStr $ unlines [ concat [ if (x,y) `S.member` bigShape then maybe " " show $ M.lookup (x,y) inputMap else " " | x <- [0..9]] | y <- [0..9] ] where
    bigShape = Data.List.foldr S.union S.empty shs

printShapesOnBoard2 :: [Shape] -> IO ()
printShapesOnBoard2 shs = putStr . (\ i -> unlines [[ M.findWithDefault ' ' (x,y) i | x <- [0..9]] | y <- [0..9]]) . M.fromList . concat $ zipWith (\ c s -> map (flip (,) c) $ toList s) symbols shs where
    symbols = ['a'..'z'] ++ ['0'..'9'] ++ repeat '?'

increasingBy :: (a -> a -> Bool) -> [a] -> [a]
increasingBy c = g where
    g (x:s) = x : g (dropWhile (not . c x) s)
    g s = s

anotater :: (a -> b) -> a -> (a, b)
anotater = (id &&&)

anotatel :: (a -> b) -> a -> (b, a)
anotatel = (&&& id)


-- mapM_ print . increasingBy ((<) `on` fst) . map (anotatel (`valuate` inputMap)) . concatMap nonOverlappingCombinations . sortBy (compare `on` length) . map (concatMap (\ s -> filter (check inputMap) . map (flip translate s) $ posds s) . uniqeVariants) $ shapes !! 5

main' :: [Shape] -> IO ()
main' = mapM_ print
    . increasingBy ((<) `on` fst) . map (anotatel (`valuate` inputMap))
    . concatMap nonOverlappingCombinations
    . reverse . sortBy (compare `on` length)
    . map (concatMap (\ s -> filter (check inputMap) . map (flip translate s) $ posds s) . uniqeVariants)

main2' :: [Shape] -> IO ()
main2' = mapM_ print
    . increasingBy ((<) `on` fst) . map (anotatel (`valuate` inputMap))
    . concatMap nonOverlappingCombinations2
    . sortBy (flip (compare `on` length))
    . map (concatMap (\ s -> filter (check inputMap) . map (flip translate s) $ posds s) . uniqeVariants)

main :: IO ()
main = do
    [n] <- getArgs
    main2' $ shapes !! (read n - 1)

orderedPerms :: [a] -> [[a]]
orderedPerms s = f [] s [] where
    f :: [a] -> [a] -> [[a]] -> [[a]]
    f a [] r = reverse a:r
    f a s r = Data.List.foldr (\ (x,t) g -> f (x:a) t . g ) id (selections s) r

orderedCombs :: [a] -> [[a]]
orderedCombs s = f [] s [] where
    f :: [a] -> [a] -> [[a]] -> [[a]]
    f a [] r = reverse a:r
    f a s r = reverse a : Data.List.foldr (\ (x,t) g -> f (x:a) t . g ) id (selections' s) r

sorted [] = True
sorted (x:s) = f x s where
    f v (y:t) = v < y && f y t
    f _ [] = True

