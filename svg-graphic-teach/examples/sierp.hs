import SimpleGraphics

sierp1 :: Int -> Graphic
sierp1 0 = rectangle 1 1
sierp1 n = overGraphics
    . zipWith translate [(x*3^(n-1),y*3^(n-1))|x<-[0..2],y<-[0..2],x/=1 || y/=1]
    . repeat . sierp1 $ n - 1

sierp2 :: Int -> Graphic
sierp2 0 = polygon [(0,0),(0,1),(1,0)]
sierp2 n = overGraphics
    . zipWith translate [(x*2^(n-1),y*2^(n-1)) | (x,y) <- [(0,0),(0,1),(1,0)]]
    . repeat . sierp2 $ n - 1


main :: IO ()
main = do
    putStrLn "Writing sierp1.svg"
    writeImage "sierp-ctverec.svg" (sierp1 5)
    putStrLn "Writing sierp2.svg"
    writeImage "sierp-trojuhelnik.svg" (sierp2 9)
