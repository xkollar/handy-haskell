module Handy.Number
    (
    -- * Modular arithmetic
      egcd
    , modInv
    -- * Arithmetic
    , sqrtBig
    -- * Often used seqences
    , diagonal
    , fibs
    , primes
    -- * Encoding/Decoding
    , enc1, dec1
    , enc2, dec2
    , encodeBase
    , encodeBij, decodeBij
    ) where

-- -- | Standard GCD
-- gcd :: Integral a => a -> a -> a
-- gcd a 0 = abs a
-- gcd a b = gcd b (a `mod` b)

-- Auxiliary function
egcd' :: Integral a => a -> a -> (a, a, a)
egcd' a 0 = (a, 1, 0)
egcd' a b = (g, t, s - d * t) where
    (d, m) = a `quotRem` b
    (g, s, t) = egcd' b m

-- | Extended Euclidean algorithm (Greatest Common Divisor).
--
-- > a*x + b*y == g && g == gcd a b where (x,y,g) = egcd a b
egcd :: Integral t => t -> t -> (t, t, t)
egcd a b = (i * g,i * s,i * t) where
    i = signum g
    (g,s,t) = egcd' a b

-- | For given @n@ finds largest @m@ such that @m^2 <= n@. Newtons method.
--
-- >>> sqrtBig 121
-- 11
-- >>> sqrtBig 120
-- 10
sqrtBig :: Integral b => b -> b
sqrtBig n = head . dropWhile ((n<).(^(2::Int))) $ iterate f n where
    f x = (x + n `div` x) `div` 2

-- for m < 0:
--   as 1 = 0 + 1
--   and m = 0
--   so 1 = m + 1
-- example:
--   modInv 2 (-13) = Just (-6)
--   2 * (-6) = (-12)
--   (-12) `mod` (-13) = (-12)
--
-- | Modular inverse.
--
-- > (a * i) `mod` m == 1 where Just i = a `modInv` m
modInv :: Integral a => a -> a -> Maybe a
modInv a m = if g == 1 then Just (i `mod` m) else Nothing where
    (g, i, _) = egcd a m

-- | List of prime numbers.
primes :: [Integer]
primes = 2:3: filter isPrime [5,7..] where
    isPrime n = f $ takeWhile (<=bound) primes where
        f = notElem 0 . map (mod n)
        bound = sqrtBig n

-- | List of Fibonacci numbers.
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- | Diagonal enumeration of pairs of natural numbers.
-- Every pair is guaranteed to occur at finite position
-- in the list. Usable mostly with types representing unbound numbers.
diagonal :: Integral a => [(a, a)]
diagonal = [(x,n-x) | n <- [0..], x <- [0..n] ]


-- | Encode tuple of natural numbers into position on diagonal enumeration.
--
-- prop> map (uncurry enc1) diagonal == [0..]
enc1 :: Integral a => a -> a -> a
enc1 x y = (x + y)*(x + y + 1) `div` 2 + x

-- | Decoding for 'enc1' encoding.
--
-- For natural numbers:
--
-- prop> dec1 . uncurry enc1 = id
--
-- prop> uncurry enc1 . dec1 = id
dec1 :: Integral a => a -> (a, a)
dec1 0 = (0,0) where
dec1 n = (c, a+d-c) where
    a = sqrtBig (n*2)
    b = n - enc1 0 a
    c = (a + b) `mod` a
    d = b `div` a

-- | Alternate encoding of tuples of natural numbers.
enc2 :: Integral a => a -> a -> a
enc2 x y = 2^x * (y*2+1)

-- | Decoding for 'enc2' encoding.
-- For natural numbers:
--
-- prop> dec2 . uncurry enc2 = id
--
-- prop> uncurry enc2 . dec2 = id
dec2 :: Integral a => a -> (a, a)
dec2 n = (x,y) where
    (x, y') = f 0 n where
        f a b = if m == 0 then f (a+1) d else (a,b) where (d, m) = b `divMod` 2
    y = (y' - 1) `div` 2

-- | Encode natural number in base @(length s)@, using symbols from @s@.
-- Provided @s@ must be of length no less than 2.
--
-- >>> encodeBase ['0'..'9'] 100
-- "100"
-- >>> encodeBase "01" 5
-- "101"
-- >>> encodeBase "0123456789abc" (6*9)
-- "42"
--
-- It is not a bijection due to \"leading zeros problem\".
encodeBase :: Integral a => [b] -> a -> [b]
encodeBase s = reverse . f where
    l = fromIntegral $ length s
    f 0 = []
    f n = s !! fromIntegral m : f d where
        (d,m) = divMod n l

-- | Bijectively encode natural numbers into sequence.
-- This is not standard math encoding, but is handy
-- when you need to use space as efficiently as you can.
--
-- Provided @s@ must be non-empty. For single element list
-- it collapses to length-encoding.
--
-- Handy for generating examples:
--
-- > map (encodeBij "aA") [1..]
encodeBij :: Integral a => [b] -> a -> [b]
encodeBij s = reverse . f where
    l = fromIntegral $ length s
    f 0 = []
    f n = s !! fromIntegral m : f d where
        (d,m) = divMod (n - 1) l

-- | Decoding for 'encodeBij' encoding.
--
-- For natural numbers and non-empty list:
--
-- prop> encodeBij s . decodeBij s = id
--
-- prop> decodeBij s . encodeBij s = id
decodeBij :: (Integral a, Eq b) => [b] -> [b] -> a
decodeBij s = f . reverse where
    l = fromIntegral $ length s
    f = foldr (\ x y -> x + y * l) 0 . map pos where
        pos = succ . fromIntegral . length . flip takeWhile s . (/=)
