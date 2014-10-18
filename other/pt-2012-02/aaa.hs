module Main ( main ) where

import Data.Function
import qualified System.IO

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

lst' :: Integer -> Integer -> Integer -> [(Rational,Integer)]
lst' s x n = zipWith (\ a b ->(fromIntegral a / b,a)) (iterate (x+) s) (repeat $ fromIntegral n)

baf :: Integer -> Integer -> Integer -> (Rational,(Rational,Integer))
baf s d = last . takeWhileCons ((>) `on` fst) . map (anotatel $ abs . polynome . fst) . lst' s d

bafBD s n = (baf s 1 n) `mx` (baf s (-1) n) where
    mx xt@(x,_) yt@(y,_) = if x < y then xt else yt

comeone i (x:s) = (k,w) : comeone a s where
    (k,(w,a)) = bafBD i x
comeone _ [] = []

cumulMin :: Ord a => [a] -> [a]
cumulMin = cm where
    cm [x] = [x]
    cm (x:s) = x : cm (dropWhile (x<=) s)

niceList :: Show a => [a] -> IO ()
niceList = putStrLn . unlines . map show

main1 :: IO ()
main1 = niceList . cumulMin $ map bestApproxFor [2..999999999]

main2 :: IO ()
main2 = niceList . cumulMin $ comeone 1 [2..999999999]

main :: IO ()
main = do
    System.IO.hSetBuffering System.IO.stdout System.IO.NoBuffering
    main2
