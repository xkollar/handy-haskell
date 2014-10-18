import Control.Arrow ((&&&))
import Data.List (sort)

-- unitCharge = 1.602176565e-19

-- perm =  625000 / (22468879468420441 * pi)

-- bigf p = (1/(4*pi*perm)) * ((0-p) / (d (0-p))^3)

-- scalar multiply
sm s (x,y) = (s*x,s*y)
-- vector sum
vs (x1,y1) (x2,y2) = (x1+x2,y1+y2)

d (x,y) = sqrt (x^2 + y^2)

-- f v = (-22468879468420441 / (4 * 62500 * d v ^ 3)) `sm` v
-- f v = (-1 / (d v ^ 3)) `sm` v
-- f p = ((unitCharge * unitCharge)/(4*pi*perm)) `sm` ((1 / (d (sm (-1) p) )^3) `sm` (sm (-1) p))
-- f v = ((unitCharge * unitCharge * 22468879468420441) / (4 * 625000 * (d p)^3)) `sm` p where
    -- p = 0.00000000000001193 `sm` v

-- main = print . d . foldr1 vs $ map f [(-2,-2),(-2,3),(4,1)]

anotatel :: (a -> b) -> a -> (b,a)
anotatel = (&&& id)

myf = (1.6202698*) . d . foldr vs (0,0) . map (\ v -> (1 / d v ^ 3) `sm` v)

sur (x,y) = [(x+dx,y+dy)|dx<-[-m..m],dy<-[-n..n],x+dx/=0,y+dy/=0,(x+dx)^2+(y+dy)^2<=1111^2] where
    m = 2
    n = 2

-- (1.3405564180725947e-11,[(-887.0,638.0),(757.0,284.0),(-524.0,-715.0)])
-- (4.196307845569575e-12,[(-288.0,-355.0),(420.0,222.0),(-351.0,640.0)])
-- let fun [p1,p2,p3] = [[np1,np2,np3]|np1<-sur p1,np2<-sur p2,np3<-sur p3] in putStr . unlines . map (show . anotatel myf) $ iterate (snd . minimum . map (anotatel myf) . fun) [(-375.0,-463.0),(543.0,296.0),(-470.0,843.0)]
-- (3.868486213543856e-11,[(-375.0,-463.0),(543.0,296.0),(-470.0,843.0)])
-- let fun [p1,p2,p3] = [[np1,np2,np3]|np1<-sur p1,np2<-sur p2,np3<-sur p3] in putStr . unlines . map (show . anotatel myf) $ iterate (snd . minimum . map (anotatel myf) . fun) [(-375.0,-463.0),(543.0,296.0),(-470.0,843.0)]
-- (3.868486213543856e-11,[(-375.0,-463.0),(543.0,296.0),(-470.0,843.0)])
--
-- 2.5094731238714516e-11,[(-375.0,-470.0),(546.0,304.0),(-476.0,852.0)]
-- (1.3962561336150048e-12,[(-396.0,-505.0),(590.0,318.0),(-482.0,893.0)])
--

main :: IO ()
main = let fun [p1,p2,p3] = [[np1,np2,np3]|np1<-sur p1,np2<-sur p2,np3<-sur p3] in
    -- putStr . unlines . map (show . anotatel myf) $ iterate (snd . head . sort . map (anotatel myf) . fun) [(-375.0,-463.0),(543.0,296.0),(-470.0,843.0)]
    putStr . unlines . map (show . anotatel myf) $ iterate (snd . minimum . map (anotatel myf) . fun) [(-1014,-454),(454,1014),(-778.0,778.0)]
