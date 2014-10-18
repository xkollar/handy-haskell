
import SimpleGraphics
import Data.List

size :: Double
size = 20

sep :: Double
sep = 2

swap = uncurry (flip (,))

-- sortTuple
st t@(x,y) = (if x <= y then id else swap) t


anotater f x = (x,f x)
anotatel f x = (f x,x)

square c x = withFill c $ rectangle x x

uniq :: Ord a => [a] -> [a]
uniq (x:s) = x : uniq (dropWhile (x==) s)
uniq [] = []

magic l s = overGraphics $ map (drawSq . anotater f) grid where
    grid = [(x,y)| y <- [ymin..ymax], x <- [xmin..xmax]]
    xmin = 1 + ymax
    -- xmax = n - n `div` 3 + ymax
    xmax = n
    ymin = 1
    ymax = n `div` 3
    drawSq ((x,y),c) = translate (mul * fromIntegral (x - xmin + 1),mul * fromIntegral y) (square c size)
    mul = size+2*sep
    f t@(x,y)
        | t `elem` s'   = Blue
        | umin t `elem` ds      = RGB 200 200 200
        | (>=3) . length $ filter ((x==).fst) s' = RGB 250 200 200
        | (>=3) . length $ filter ((y==).snd) s' = RGB 250 200 200
        | not . null $ intersect s' [ (x',y') | (_,y') <- (filter ((x==).fst) s'), (x',_) <- (filter ((y==).snd) s')] = RGB 250 200 250
        | otherwise        = None
    s' = uniq $ map (swap . st) s
    n = 2^l - 1
    ds = uniq . sort $ map umin s'
    umin = abs . uncurry (-)


addSym l s = concatMap f s' where
    s' = uniq $ map (swap . st) s
    xmin = 1 + ymax
    -- xmax = n - n `div` 3 + ymax
    xmax = n
    ymin = 1
    ymax = n `div` 3
    n = 2^l - 1
    f (x,y) = [(x,y),(xmax - x + xmin,ymax - y + ymin)]

-- significant = writeImage "t3.svg" . withStroke Black 1 . magic 6 . addSym 6 $ [(11,22),(21,22),(20,22),(20,23),(21,25),(18,23),(19,25),(20,27),(21,29),(16,25),(13,23),(17,29),(14,27),(15,29),(12,27)]
