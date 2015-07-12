module Main ( main ) where

import SimpleGraphics

spiralize :: Num t => t -> [(t, t)] -> [(t, t)]
spiralize step s = l where
    l = s ++ zipWith g (tail l) (tail $ tail l)
    g (x1,y1) (x2,y2) = ((x2-x1)*step+x1,(y2-y1)*step+y1)

spiralPoints1 :: [Point]
spiralPoints1 = take 1000 $ spiralize 0.05 [(0,0),(400,0),(0,400),(0,0)]

spiralPoints2 :: [Point]
spiralPoints2 = take 1000 $ spiralize 0.05 [(400,0),(400,400),(0,400),(400,0)]

basicSpiral :: Graphic
basicSpiral = withFill None . withStroke Black 1 . polyline $ reverse spiralPoints1 ++ spiralPoints2

main :: IO ()
main = writeImage "basic-triangles.svg" basicSpiral
