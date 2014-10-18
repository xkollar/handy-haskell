module Main ( main ) where

import Data.Function

polynome :: Rational -> Rational
polynome p = polynome' (1-p) where
    polynome' t = 0.5 - t + t^13 - t^15 + t^18 - t^20 + t^22 - t^23

anotater :: (a -> b) -> a -> (a,b)
anotater f x = (x,f x)

anotatel :: (a -> b) -> a -> (b,a)
anotatel f x = (f x,x)

-- md f l u = if l == u then l else md where
--     n = (l + u) `div` 2

takeWhileCons p = twc where
    twc (x:y:s) = if p x y then x : twc (y:s) else [x]
    twc [x] = [x]
    twc [] = []

lst :: Integer -> [Rational]
lst n = zipWith (/) (map fromIntegral . takeWhile (>0) . iterate (-1+) $ n `div` 2) (repeat $ fromIntegral n)

bestApproxFor :: Integer -> (Rational,Rational)
bestApproxFor = last . takeWhileCons ((>) `on` fst) . map (anotatel $ abs . polynome) . lst

cumulMin :: Ord a => [a] -> [a]
cumulMin = cm where
    cm [x] = [x]
    cm (x:s) = x : cm (dropWhile (x<=) s)

niceList :: Show a => [a] -> IO ()
niceList = putStrLn . unlines . map show

main :: IO ()
main = niceList . cumulMin $ map bestApproxFor [2..999999999]

