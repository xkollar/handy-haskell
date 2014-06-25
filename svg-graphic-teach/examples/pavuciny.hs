import SimpleGraphics

siet :: Double -> Double -> Double -> Double -> Graphic
siet n u c o = spirala n u c `overGraphic` luce (n*c+o) u where
    spirala n u c = polyline $ map f [0..n] where
        f x = (mul * cos pos,mul * sin pos) where
            mul = c * x
            pos = x * pi * 2 / u
    luce c n = overGraphics $ map (line (0,0) . f ) [1..n] where
        f x = (c * cos pos,c * sin pos) where
            pos = x * 2 * pi / n

main :: IO ()
main = do
    putStrLn "Writing pavuciny.svg"
    writeImage "pavuciny.svg" . withFill None . withStroke Red 1
        $ translate (100,100) (siet 50 13 1.5 10)
        `overGraphic` translate (300,100) (siet 50 7 1.5 10)
        `overGraphic` translate (200,300) (siet 100 15 1.1 10)

