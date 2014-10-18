import System.IO

-- f = g 2
-- g a 1 = []
-- g a n = if n `mod` a == 0 then a : g a (n `div` a) else g (a+1) n
-- h x = [y | y<-[1..x], x `mod` y == 0]
-- i = j 0
-- j a (x:y:s) = if x == y then j (a+1) (y:s) else (a+1) : j 0 (y:s); j a [_] = [a+1]
-- j _ [] = []
-- k = product . map (1+) . i . f
-- main = putStrLn . concatMap (\ x -> if all even . i . f $ x then show x ++ "," else ".") . id $ (filter (\ x -> x `mod` 1000 == 576) $ scanl (+) 1 [2..])
-- let f ((x:s):xss) = x : zipWith (.) s (f xss); lamps = zipWith (flip id) (repeat False) . f $ map (\ n -> cycle (not : replicate (n-1) id)) [1..] in putStrLn $ concatMap (\ x -> if x then "X" else "_") lamps

-- sqrtBig :: Integer -> Integer
--sqrtBig n = head . dropWhile ((n<).(^2)) $ iterate f (n`div`2) where
sqrtBig n = head . dropWhile ((n<).(^2)) $ iterate f n where
    f x = (x + n `div` x) `div` 2

isSquare :: Integer -> Bool
isSquare n = n == sn * sn where
    sn = sqrtBig n

factors :: [Integer] -> Integer -> [Integer]
factors ps x = f (takeWhile (<= sqrtBig x) ps) x where
    f _ 1 = []
    f xs@(x:s) n = if m == 0 then x : f xs d else f s n where
        (d,m) = divMod n x
    f [] n = [n]

primes :: [Integer]
primes = 2 : [ x | x <- [3..], [_] <- [factors primes x]]

primes' = sieve [2..] where
    sieve (x:s) = x : sieve (filter ((0/=) . flip mod x) s)

i :: [Integer] -> [Integer]
i = j 0 where
    j a (x:y:s) = if x == y then j (a+1) (y:s) else (a+1) : j 0 (y:s)
    j a [_]     = [a+1]
    j _ []      = []

dragonPositions :: [Integer]
-- dragonPositions = filter f $ scanl (+) 1 [2..] where
--     f x = x `mod` 1000 == 576 && x `mod` 16 == 0
dragonPositions = filter p . map f $ concatMap (\x -> [32 * x - 1, 32 * x]) [1,3..] where
    f n = (n*(n+1)) `div` 2
    p x = x `mod` 1000 == 576

progressFind :: (Integer -> Bool) -> [Integer] -> IO ()
progressFind p = putStrLn . concatMap (\ x -> if p x then show x ++ "," else ".")

main :: IO ()
main = do
    -- hSetBuffering stdout NoBuffering
    -- progressFind (all even . i . factors primes) dragonPositions
    progressFind isSquare dragonPositions

{-
filter isSquare $ scanl (+) 1 [2..] ~>
[1,36,1225,41616,1413721,48024900,1631432881,55420693056,1882672131025,...
   1^2 =       1 =    1 *    2 / 2
   6^2 =      36 =    8 *    9 / 2
  35^2 =    1225 =   49 *   50 / 2
 204^2 =   41616 =  288 *  289 / 2
1189^2 = 1413721 = 1681 * 1682 / 2

-}
