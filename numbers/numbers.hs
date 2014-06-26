import Prelude hiding ( gcd )

gcd :: Integral a => a -> a -> a
gcd a 0 = a
gcd a b = gcd b (a `mod` b)

diagonal :: [(Integer, Integer)]
diagonal = [(x,n-x) | n <- [0..], x <- [0..n] ]

sqrtBig :: Integral b => b -> b
sqrtBig n = head . dropWhile ((n<).(^(2::Int))) $ iterate f n where
    f x = (x + n `div` x) `div` 2

-- | map (uncurry enc1) diagonal == [0..]
enc1 :: Integral a => a -> a -> a
enc1 x y = (x + y)*(x + y + 1) `div` 2 + x

-- | dec1 . uncurry enc1 == id
dec1 :: Integral a => a -> (a, a)
dec1 0 = (0,0) where
dec1 n = (c, a+d-c) where
    a = sqrtBig (n*2)
    b = n - enc1 0 a
    c = (a + b) `mod` a
    d = b `div` a

enc2 :: Integral a => a -> a -> a
enc2 x y = 2^x * (y*2+1)

dec2 :: Integral a => a -> (a, a)
dec2 n = (x,y) where
    (x, y') = f 0 n where
        f a b = if m == 0 then f (a+1) d else (a,b) where (d, m) = b `divMod` 2
    y = (y' - 1) `div` 2

-----------------

primes :: [Integer]
primes = 2:3: filter isPrime [5,7..] where
    isPrime n = f $ takeWhile (<=bound) primes where
        f = notElem 0 . map (mod n)
        bound = sqrtBig n

-----------------

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
