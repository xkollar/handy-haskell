module Main (main) where

import Control.Arrow
import Data.Function (on)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Tuple
import Text.Printf

import Data.Map ((!))

import Handy
import qualified SimpleGraphics as G

import Graph (Graph, fromList, fsp)

-- main :: IO ()
-- main = do
--     print 42
--     G.writeImage "/tmp/test.svg"
--         . G.translate (50,50) . G.scale 2
--         . G.overGraphic (grPoints $ call swingsAroundFromDir (9,7) (4,12) CounterClockWise)
--         $ G.withStroke G.Black 0.1 base

data Direction = ClockWise | CounterClockWise deriving (Eq, Show)

type Point = (Int, Int)

type Vector = Point

type Angle = (Int,Int)
type SqDistance = Int

vector :: Point -> Point -> Vector
vector (sx,sy) (ex,ey) = (ex-sx,ey-sy)

translate :: Vector -> Point -> Point
translate (dx,dy) (x,y) = (x+dx,y+dy)

-- Input for the Puzzle
--     abcdefghijklmnopqrst
input0 :: [[Char]]
input0 =
    [ "    o      o o     2" -- 20
    , "  o   o o         o " -- 19
    , "              o     " -- 18
    , "    o o     o  o    " -- 17
    , "   o   o  o       o " -- 16
    , "  o        o       o" -- 15
    , "                    " -- 14
    , " o       o   o      " -- 13
    , "   o      o     o   " -- 12
    , "o           o     o " -- 11
    , " o      o           " -- 10
    , "   o o    o         " -- 09
    , "        o           " -- 08
    , "o                  o" -- 07
    , "                    " -- 06
    , " o         o       o" -- 05
    , "o o    o     o      " -- 04
    , "         o          " -- 03
    , "      o   o       o " -- 02
    , "1 o o         o     " -- 01
--     abcdefghijklmnopqrst
    ]

-- Example input Puzzle
--     abcdefghijkl
input1 :: [[Char]]
input1 =
    [ "   1        " -- 12
    , "            " -- 11
    , "            " -- 10
    , "            " -- 09
    , " x          " -- 08
    , "        o   " -- 07
    , " 2          " -- 06
    , "            " -- 05
    , "     o x    " -- 04
    , "      o     " -- 03
    , "   x   x    " -- 02
    , "            " -- 01
--     abcdefghijkl
    ]

input2 :: [[Char]]
input2 =
    [ "                                       " -- 20
    , "                                       " -- 19
    , "                                       " -- 18
    , "                                       " -- 18
    , "                                       " -- 18
    , "                                       " -- 18
    , "                                       " -- 18
    , "1                                      " -- 17
    , "x                                      " -- 16
    , "x                                      " -- 15
    , "x                                      " -- 14
    , "xxxx                                   " -- 13
    , "   x                                   " -- 12
    , "   x                                   " -- 11
    , "   x                                   " -- 10
    , "   xxx?                                " -- 09
    , "      x            oxxxxoo             " -- 08
    , "      x            x    x              " -- 07
    , "      x            x    x              " -- 06
    , "      xxxx     oxxxx    xxxx?          " -- 05
    , "         x     x                       " -- 04
    , "         x     x                       " -- 03
    , "         x     x                       " -- 02
    , "         xxxoxxx                       " -- 01
    ]

input :: [[Char]]
input = input0

height :: Int
height = length input

width :: Int
width = maximum $ 0 : map length input

-- for enumerating points in rotational ((counter)clockwise) order...
-- (2*) for it can be diagonal and it is smallest nat larger than (sqrt 2)
infinity :: Int
infinity = 2 * max height width

list2map :: [[a]] -> [(Point, a)]
list2map = concat . zipWith (\ y -> zipWith (\ x -> (,) (x,y)) [1..]) [1..]

showPoint :: Point -> String
showPoint (x,y) = show' x ++ show y where
    show' n = take 1 $ drop n ('_':['a'..])

newtype ShowablePoint = ShowablePoint Point

instance Show ShowablePoint where
    show (ShowablePoint p) = showPoint p

-- Reverse, so it matches original assignment
inputMap :: [(Point, Char)]
inputMap = list2map $ reverse input

findMap :: (Char -> Bool) -> [Point]
findMap p = map fst $ filter (p . snd) inputMap

isPostChar :: Char -> Bool
isPostChar = ('o'==)

posts :: [Point]
posts = findMap isPostChar

nonPosts :: [Point]
nonPosts = findMap (not . isPostChar)

isPost :: Point -> Bool
isPost = flip elem posts

start :: Point
start = head $ findMap ('1'==)

end :: Point
end = head $ findMap ('2'==)

det :: Point -> Point -> Point -> Int
det (x1,y1) (x2,y2) (x3,y3) =
    x1*(y2 - y3) + x2*(y3 - y1) + x3*(y1 - y2)

-- Are 3 points colinear?
colinear :: Point -> Point -> Point -> Bool
-- colinear (x1,y1) (x2,y2) (x3,y3) =
--     x1*(y2 - y3) + x2*(y3 - y1) + x3*(y1 - y2) == 0
colinear p1 p2 p3 = det p1 p2 p3 == 0

-- cond (x1,y1) (x2,y2) (x3,y3) = (y2 - y1)*(y3 - y1)
-- q1comp (x1,y1) (x2,y2) (x3,y3) = (x2 - x1)(y3 -

-- rotate 90 degrees around 0 counterclockwise
rot090 :: Point -> Point
rot090 (x,y) = (-y,x)

-- Vectors that go counterclockwise around (0,0)
vectorsCounter :: [Vector]
vectorsCounter = concat . take 4 . iterate (map rot090)
    . map swap . reverse $ sortBy cmpQ1
        [ (x,y) | x <- [1..infinity], y <- [0..infinity] ]
    where
    -- cmpQ1 = (\ p1 p2 -> compare 0 $ det (0,0) p1 p2)
    cmpQ1 p1 p2 = compare 0 $ det (0,0) p1 p2

-- Some lentghs might be non-natural, but as distances are whole numbers,
-- r^2 = dx^2 + dy^2, so squared will be natural.
distanceSq :: Point -> Point -> Int
distanceSq (x1,y1) (x2,y2) = (x1 - x2)^2 + (y1 - y2)^2

-- To filter out circle from all points provided
closerEqThanSq :: Point -> Int -> [Point] -> [Point]
closerEqThanSq p dsq = filter $ (<=dsq) . distanceSq p

rotListTo  :: (a -> Bool) -> [a] -> [a]
rotListTo p = uncurry (flip (++)) . break p

pointsAroundFromDir :: Point -> Angle -> SqDistance -> Direction -> [Point]
pointsAroundFromDir c a dsq d = map (translate c)
    . rotListTo predicate
    . closerEqThanSq (0,0) dsq
    $ f vectorsCounter
    where
    -- predicate v = sameQuad v a && (a == v) -- refine
    predicate v = sameQuad v a && sameQuadTest
        where
        sameQuadTest = if d == CounterClockWise
            then 0 <= det (0,0) a v
            else 0 >= det (0,0) a v
    f = if d == CounterClockWise then id else map swap

sameQuad :: Point -> Point -> Bool
sameQuad (ax,ay) (bx,by) =
    -- XXX
    ax ~~ bx && ay ~~ by
    -- positive ax == positive bx
    -- && positive ay == positive by
    where
    positive x = x >= 0
    (~~) = (==) `on` positive

-- Test with
-- (\ d dl c -> let trn (x,y) (dx,dy) = (x+dx,y+dy) :: (Int,Int); problem p v = pointsAroundFromDirBounded p $ trn p v in writeImage "/tmp/test.svg" $ translate (50,50) . scale 2 . overGraphic (grPoints $ problem c dl d) . withStroke Black 0.1 $ base) CounterClockWise (6,-4) (10,7)
pointsAroundFromDirBounded
    :: Point -> Angle -> SqDistance -> Direction -> [Point]
pointsAroundFromDirBounded c a dsq' d =
    takeWhile inside $ pointsAroundFromDir c a dsq' d
    where
    inside p = betweenLR && betweenBT
        where

        betweenLR = if x >= 0
            then 0 <= rb*rb*(x*x + y*y) - dsq*x*x
            else 0 <= lb*lb*(x*x + y*y) - dsq*x*x
        betweenBT = if y >= 0
            then 0 <= tb*tb*(x*x + y*y) - dsq*y*y
            else 0 <= bb*bb*(x*x + y*y) - dsq*y*y

        dsq = 4 * dsq'
        transf (x',y') = ((x'-cx)*2,(y'-cy)*2)
        (x,y) = transf p
        (cx,cy) = c
        lb = 2*(     0 - cx) + 1
        rb = 2*( width - cx) + 1
        bb = 2*(     0 - cy) + 1
        tb = 2*(height - cy) + 1

isSquare :: Integral a => a -> Bool
isSquare x = x == sq (sqrtBig x) where sq x = x*x

-- (sqrt(r1sq) - sqrt(r2sq))^2
-- = sqrt(r1sq)^2 - 2*sqrt(r1sq)*sqrt(r2sq) + sqrt(r2sq)^2
-- = r1sq - 2*sqrt(r1sq)*sqrt(r2sq) + r2sq
-- = r1sq + r2sq - 2*sqrt(r1sq)*sqrt(r2sq)
-- = r1sq + r2sq - 2*sqrt(r1sq)*sqrt(r2sq)
-- = r1sq + r2sq - 2*sqrt(r1sq*r2sq)
--
-- also as we are counting only with integers, we can not get non-integer
-- result, so it is OK to work with... IS IT? can two swings cancel this
-- effect? -- Seems ok, as whole path can be made of segments with
-- whole-numbered square.
swingsAroundFromDir :: Point -> Angle -> SqDistance -> Direction -> [Point]
swingsAroundFromDir c a dsq d = nub . f $ pointsAroundFromDirBounded c a dsq d
    where
    f s = filter (\p -> dsq == distanceSq p c) bi ++ rest bt
    -- f s = bi ++ rest bt
        where
        rest [] = []
        rest (p:_) = if dsq == dsq2 || not (isSquare dsq12)
            then [] -- We hit post... or reziduum is not square
            else swingsAroundFromDir p (vector c p) (dsq + dsq2 - 2*sqrtBig dsq12) d
                -- We are swinging from new post
            where
            dsq2 = distanceSq c p
            dsq12 = dsq * dsq2
        (bi,bt) = break isPost s

swingsAroundFrom :: Point -> Angle -> SqDistance -> [Point]
swingsAroundFrom c a dsq =
    nub $ reverse (swingsAroundFromDir c a dsq ClockWise)
    ++ swingsAroundFromDir c a dsq CounterClockWise

sortByDistanceFrom :: Point -> [Point] -> [Point]
sortByDistanceFrom p = sortBy (compare `on` distanceSq p)

-- Posts that do not have colinear post closer to p.
visiblePosts :: Point -> [Point]
visiblePosts p = f [] $ sortByDistanceFrom p posts where
    f a [] = reverse a
    f a (x:s) = if any foo a then f a s else f (x:a) s
        where
        foo q = colinear p x q && sameQuad (vect p q) (vect p x)
        vect (x1,y1) (x2,y2) = (x2-x1, y2-y1)

-- swings :: Direction -> Point -> Point -> [Point]

-- Graphics
gscale :: Double
gscale = 10

point2gpoint :: Point -> G.Point
point2gpoint (x,y) =
    (fromIntegral x * gscale, fromIntegral (height - y + 1) * gscale)

circ :: G.Vector -> G.Graphic
circ p =
    G.withFill G.None . G.withStroke G.Red 0.1 . G.translate p $ G.circle 3

grPoints :: [Point] -> G.Graphic
grPoints s = G.overGraphics (map circ ps) `G.overGraphic` gl ps where
    ps = map point2gpoint s
    gl = G.withFill G.None . G.withStroke G.Green 0.1 . G.polyline

base :: G.Graphic
base = gposts `G.overGraphic` frame where
    frame = G.withFill G.None $ G.polyline
        [(zer,zer), (zer, h), (w,h), (w, zer), (zer,zer)]
        where
        zer = 0.5 * gscale
        w = (fromIntegral width + 0.5) * gscale
        h = (fromIntegral height + 0.5) * gscale
    gposts = G.overGraphics . map (flip G.translate $ G.circle 2)
        $ map point2gpoint posts

call :: (Point -> Vector -> SqDistance -> t) -> Point -> Point -> t
call f c s = f c (vector c s) (distanceSq c s)

-- writeImage "/tmp/test.svg" $ translate (50,50) . scale 2 . overGraphic (grPoints $ pointsAroundFromDir (10,10) (5,5) CounterClockWise) . withStroke Black 0.1 $ base


-- Graph

type Price = Rational
type MyGraph = Graph Point Price

price :: Point -> Point -> Price
price p1 p2 = if dsq == 0 then 0 else 1 / fromIntegral dsq
    where
    dsq = distanceSq p1 p2

graph :: MyGraph
graph = fromList $ concatMap (\place ->
    concatMap (\post -> map (\x -> (place,x,price place post))
    $ call swingsAroundFrom post place) $ visiblePosts place) nonPosts

postForSwing :: Point -> Point -> Point
postForSwing start target = snd . minimumBy (compare `on` fst)
    . map snd . filter ((target==) . fst)
    . concatMap (\post -> map (\swing -> (swing, (price start post,post))) $ call swingsAroundFrom post start)
    $ visiblePosts start

magic :: (a -> a -> b) -> [a] -> [b]
-- magic f = rec where
--     rec (x:s@(y:_)) = f x y : rec s
--     rec _ = []
magic f s = zipWith f s (tail s)

main :: IO ()
main = do
    putStrLn $ kukPrice thePrice
    printf "%0.4f\n" $ (fromRational thePrice :: Double)
    mapM_ (putStrLn . kukSwing) thePath
    where
    (thePrice, thePath) =
        second (magic (\ a b -> ((a,postForSwing a b),b)))
        $ fsp start end graph
    kukPrice = show
    -- kukSwing ((_,p),e) = "(" ++ showPoint p ++ "," ++ showPoint e ++ ")"
    kukSwing ((_,p),e) = show (ShowablePoint p, ShowablePoint e)

-- 0.7097
-- (a4,c3)
-- (k2,d6)
-- (t7,d8)
-- (e1,j6)
-- (s2,o11)
-- (g2,p10)
-- (c4,q7)
-- (e20,r8)
-- (i10,r12)
-- (l15,r18)
-- (s19,t20)
--
-- To be exact (109126520238262 / 153774593213475).
--
-- What I do not understand is why would you present problem of finding
-- shortest path in a (I admit, in interesting way presented) graph (with
-- less than 400 vertices) as a optimization problem? Yes, it is possible
-- to err in figuring out underlying graph, but still... I would like you
-- to elaborate on that in your solution.
--
-- Thanks for the fun :-), I am looking forward to next month's puzzle.


diver x y = d : diver (m*10) y
    where
    (d,m) = divMod x y

-- G.writeImage "/tmp/test.svg" $ G.translate (50,50) . G.scale 2
-- . G.overGraphic (grPoints $ call swingsAroundFrom (13,1) start)
-- . G.withStroke G.Black 0.1 $ base

-- Official solution:
--
-- Very open-ended puzzle this month! The best solution we found to this
-- month’s puzzle, along with 10 of you, had a cost of about 0.7082. There
-- are actually a couple of ways to achieve that cost, but one is:
--
-- (a4,c3)
-- (k2,d6)
-- (t7,d8)
-- (e1,j6)
-- (t7,j8)
-- (q12,r4)
-- (i8,r12)
-- (l15,r18)
-- (s19,t20)
--
--  00000000011111111112222222
--  12345678901234567890123456
--  abcdefghijklmnopqrstuvwxyz
officialSolution :: [(Point,Point)]
officialSolution =
    [ (( 1,  4), ( 3,  3)) -- ( a4,  c3) -- 1 % 9
    , ((11,  2), ( 4,  6)) -- ( k2,  d6) -- 1 % 65
    , ((20,  7), ( 4,  8)) -- ( t7,  d8) -- 1 % 257
    , (( 5,  1), (10,  6)) -- ( e1,  j6) -- 1 % 50
    , ((20,  7), (10,  8)) -- ( t7,  j8) -- 1 % 101
    , ((17, 12), (18,  4)) -- (q12,  r4) -- 1 % 65
    -- ^ I somehow missed this possibility? j8 -(q12)-> r4
    , (( 9,  8), (18, 12)) -- ( i8, r12) -- 1 % 97
    , ((12, 15), (18, 18)) -- (l15, r18) -- 1 % 45
    , ((19, 19), (20, 20)) -- (s19, t20) -- 1 % 2
    ]
-- > mapM_ print . map (ShowablePoint *** ShowablePoint) $ officialSolution
-- (a4,c3)
-- (k2,d6)
-- (t7,d8)
-- (e1,j6)
-- (t7,j8)
-- (q12,r4)
-- (i8,r12)
-- (l15,r18)
-- (s19,t20)

from p [] = []
from p ((via,e):s) = (p,via) : from e s

-- > fromRational . sum . map ((1/) . (fromIntegral :: Int -> Rational) . uncurry distanceSq) $ from start officialSolution
-- 0.708203883135747

-- main = do
--     print 42
--     -- print $ graph ! (10,8) ! (18,4)
--     G.writeImage "/tmp/test.svg"
--         . G.translate (50,50) . G.scale 2
--         -- . G.overGraphic (grPoints $ call swingsAroundFrom (17,12) (10,8))
--         . G.overGraphic (grPoints $ visiblePosts (10,8))
--         $ G.withStroke G.Black 0.1 base
