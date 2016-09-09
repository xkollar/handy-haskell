module Test where

import Handy

sq x = x*x

isSquare x = sq sqrtx == x where sqrtx = sqrtBig x

-- sum of squares
-- sos n = [(x,y) | let l = sqrtBig n + 1, x <- [0..l], y <- [0..l], x*x+y*y == n]
sos n = filter (isSquare . snd) . map (annotater (n-)) . takeWhile (<= (div n 2+1)) $ map sq [0..]

isSos = not . null . sos

xys :: [(Integer,Integer)]
xys = drop 1 [(x,y)| x <- [0..20], y <- [0..x]]
sqsum x y = x*x + y*y

epsilon = 0.000005
closeEnough x = abs (x - fromIntegral (round x)) < epsilon

f x y = fromIntegral $ sqsum x y

bigR r1sq r2sq r3sq
    = fromIntegral (r1sq + r2sq + r3sq) + 2*(sqrt'(r1sq*r2sq) + sqrt'(r1sq*r3sq) + sqrt'(r2sq*r3sq))
    where
    sqrt' = sqrt . fromIntegral
    -- r1sq = fromIntegral r1sq'
    -- r2sq = fromIntegral r2sq'
    -- r3sq = fromIntegral r3sq'

list =
    [ ((x1,y1,r1sq),(x2,y2,r2sq),(x3,y3,r3sq))
    | (x1,y1) <- xys, let r1sq = sqsum x1 y1
    , (x2,y2) <- xys, let r2sq = sqsum x2 y2
    , (x3,y3) <- xys, let r3sq = sqsum x3 y3
    -- , let rsq = r1sq + r2sq + r3sq + 2*(sqrt(r1sq*r2sq) + sqrt(r1sq*r3sq) + sqrt(r2sq*r3sq))
    , let rsq = bigR r1sq r2sq r3sq
    , closeEnough rsq
    , isSos $ round rsq
    , not . isSquare $ r1sq * r2sq]

magic :: (a -> a -> b) -> [a] -> [b]
magic f = rec where
    rec (x:s@(y:_)) = f x y : rec s
    rec _ = []

magic' :: (a -> a -> b) -> [a] -> [b]
magic' f s = zipWith f s (tail s)
