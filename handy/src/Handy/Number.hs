module Handy.Number
    (
    -- * Modular arithmetic
      egcd
    , modInv
    -- * Arithmetic
    , sqrtBig
    -- * Some usefull numerical seqences
    , primes
    , fibs
    ) where

-- -- | Standard GCD
-- gcd :: Integral a => a -> a -> a
-- gcd a 0 = abs a
-- gcd a b = gcd b (a `mod` b)

egcd' :: Integral a => a -> a -> (a, a, a)
egcd' a 0 = (a, 1, 0)
egcd' a b = (g, t, s - d * t) where
    (d, m) = a `quotRem` b
    (g, s, t) = egcd' b m

-- | 'egcd a b = (x,y,g) => a*x + b*y = g && g = gcd a b'
egcd :: Integral t => t -> t -> (t, t, t)
egcd a b = (i * g,i * s,i * t) where
    i = signum g
    (g,s,t) = egcd' a b

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
modInv :: Integral a => a -> a -> Maybe a
modInv a m = if g == 1 then Just (i `mod` m) else Nothing where
    (g, i, _) = egcd a m

-- | List of prime numbers
primes :: [Integer]
primes = 2:3: filter isPrime [5,7..] where
    isPrime n = f $ takeWhile (<=bound) primes where
        f = notElem 0 . map (mod n)
        bound = sqrtBig n

-- | List of Fibonacci numbers
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

