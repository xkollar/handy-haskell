
ppList :: Show a => [a] -> IO ()
ppList = putStr . unlines . map show

-- f v p (x:s) = (x,not (null v') && x == head v',v,p) : f v' p' s where
f :: Integral t => [t] -> [t] -> [t] -> [(t, Bool)]
f v p (x:s) = (x,ev) : f v' p' s where
    t = [(m,n) | m <- [0..(x - 2) `div` 2], let n = x - 2 - m]
    v' = if ev then x:v else v
    p' = if ep then x:p else p
    ev = any fst gt
    ep = any snd gt
    gt = map g t
    -- -- g (l,r) = ((vl,vp),(pl,pp)) where
    g (l,r) = ultraMagic vl vp pl pp where
        vl = l `elem` v
        vp = r `elem` v
        pl = l `elem` p
        pp = r `elem` p

ultraMagic False False False False = (False,False)
ultraMagic False False False True  = (False,False)
ultraMagic False False True  False = (False,False)
ultraMagic False False True  True  = (True ,False)
ultraMagic False True  False False = (False,False)
ultraMagic False True  False True  = (True ,False)
ultraMagic False True  True  False = (False,True )
ultraMagic False True  True  True  = (False,False)
ultraMagic True  False False False = (False,False)
ultraMagic True  False False True  = (False,True )
ultraMagic True  False True  False = (True ,False)
ultraMagic True  False True  True  = (False,False)
ultraMagic True  True  False False = (True ,False)
ultraMagic True  True  False True  = (False,False)
ultraMagic True  True  True  False = (False,False)
ultraMagic True  True  True  True  = (False,False)

magic :: [(Integer, Bool)]
magic = f [] [0,1] [2..]

main :: IO ()
main = ppList . filter (null . filter (/='1') . filter (/='0') . show) . map ((+2) . fst) . filter (not.snd) $ magic
-- 11
-- 100
-- 101
-- 110
-- 1001
-- 1010
-- 1011
-- 10101
-- 10110
-- 10111
-- 11001
-- 11011
-- 11100
