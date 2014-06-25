import SimpleGraphics

tree1 :: Int -> Graphic
tree1 0 = withStroke Green 1 $ line (0,0) (0,3)
tree1 n = withStroke Black (fromIntegral n) (line (0,0) (0,len))
    `overGraphic` translate (0,len) (rotate angle subTree)
    `overGraphic` translate (0,len) (rotate (-angle) subTree) where
    angle  = 3 + 6 * fromIntegral n
    len = 7 * fromIntegral n
    subTree = tree1 (n-1)

tree2 :: Int -> Graphic
tree2 = t1 where
    t1 0 = emptyGraphic
    t1 n = telo
        `overGraphic` (translate (-1,30) . scale 0.7 . rotate 45 $ t1 (n-1))
        `overGraphic` (translate (0,33) . scale 0.55 . rotate (-50) $ t2 (n-1))
        where
        telo =  polygon [(0,0),(0,40),(9,45),(10,10)]
    t2 0 = emptyGraphic
    t2 n = telo
        `overGraphic` (translate (0,30) . scale 0.6 . rotate 50 $ t1 (n-1))
        `overGraphic` (translate (1,32) . scale 0.7 . rotate (-45) $ t2 (n-1))
        where
        telo =  polygon [(0,0),(0,40),(-9,45),(-10,10)]

kvietok :: Graphic
kvietok = withFill (RGB 255 255 210) (circle 15)
    `overGraphic` withFill (RGB 255 200 200) (overGraphics lupene) where
    lupene = zipWith translate pozicie . repeat $ circle 20
    pozicie =
        [ (25,0)
        , (7.72542485937369,23.7764129073788)
        , (-20.2254248593737,14.6946313073118)
        , (-20.2254248593737,-14.6946313073118)
        , (7.72542485937368,-23.7764129073788)
        ]

tree3 :: Int -> Graphic
tree3 (-1) = kvietok
tree3 0 = emptyGraphic
tree3 n =         (translate (-1,40) . scale 0.7 . rotate 45 . translate (-10,-10) $ tree3 (n-1))
    `overGraphic` (translate (0,40) . scale 0.6 . rotate (-90) $ tree3 (n-2))
    `overGraphic` telo
    where
    telo = polygon [(0,0),(0,40),(7,36),(10,10)]

main :: IO ()
main = do
    putStrLn "Writing frac-tree1.svg"
    writeImage "frac-tree1.svg" (translate (300,400) . scale (-2) . withStroke Black 1 $ tree1 7)
    putStrLn "Writing frac-tree2.svg"
    writeImage "frac-tree2.svg" (translate (200,200) . scale (-2) $ tree2 8)
    putStrLn "Writing frac-tree3.svg"
    writeImage "frac-tree3.svg" (translate (300,450) . scale (-5) . withFill (RGB 70 30 10) $ tree3 11)

