-- |
-- Module:       $HEADER$
-- Description:  Handy functions for manipulating numbers.
--
-- Handy functions for manipulating numbers.
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
    -- * Factorization
    , factor
    -- * Encoding/Decoding
    , enc1, dec1
    , enc2, dec2
    , encodeBase, decodeBase
    , encodeBij, decodeBij
    -- * Convenience functions/values
    , binBase
    , octBase
    , decBase
    , hexBase
    ) where

import Control.Arrow ((***))
import Data.Maybe (fromJust)

-- Available in Prelude...
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

-- Rho Algorithm from Wikipedia:
-- The algorithm takes as its inputs @n@, the integer to be factored;
-- and @g(x)@, a polynomial @p(x)@ computed modulo @n@.
-- This will ensure that if @p|n@, and @x ≡ y mod p@, then @g(x) ≡ g(y) mod p@.
-- In the original algorithm, @g(x) = (x2 - 1) mod n@, but nowadays it is more
-- common to use @g(x) = (x2 + 1) mod n@. The output is either a non-trivial
-- factor of @n@, or failure. It performs the following steps:[2]
--
-- > x ← 2; y ← 2; d ← 1;
-- > While d = 1:
-- >     x ← g(x)
-- >     y ← g(g(y))
-- >     d ← gcd(|x - y|, n)
-- > If d = n, return failure.
-- > Else, return d.
--
-- Note that *this algorithm may fail* to find a nontrivial factor even
-- when @n@ is composite. In that case, you can try again, using a starting
-- value other than 2 or a different @g(x)@. The name ρ algorithm comes from
-- the fact that the values of @x (mod d)@ eventually repeat with period @d@,
-- resulting in a ρ shape when you graph the values.
rho :: Integer -> Maybe Integer
rho n = f . head
    . dropWhile (1==)
    . map (gcd n . abs . uncurry (-))
    . tail $ iterate (g *** g . g) (2,2)
    where
    g x = (x*x + 1) `mod` n
    f x = if x == n then Nothing else Just x

-- coprime :: Integer -> Integer -> Bool
-- coprime m n = gcd m n == 1

-- | Reasonably fast, but due to the fact that it uses rho function for
-- numbers not divisible by first 1000 primes, resulting list may contain
-- also composites @:-(@.
factor :: Integer -> [Integer]
factor = f (take 1000 primes) where
    f _ 1 = []
    f s@(p:ps) n = if m == 0 then p : f s d else f ps n where
        (d,m) = n `divMod` p
    f [] n = g n
    g n = case rho n of
        Nothing -> [n]
        -- Just x -> g x ++ g (n `div` x)
        Just x -> f (g x) n

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
dec1 0 = (0,0)
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

-- | Decode natural number in base @(length s)@, using symbols from @s@.
-- Provided @s@ must be of length no less than 2. It is dual to 'encodeBase'.
--
-- >>> decodeBase ['0'..'9'] "101" :: Int
-- 101
-- >>> encodeBase "01" "101" :: Int
-- 5
--
-- It is not a bijection due to \"leading zeros problem\".
--
-- prop> decodeBase s . encodeBase s = id
decodeBase :: (Integral a, Eq b) => [b] -> [b] -> a
decodeBase s = f . reverse where
    l = fromIntegral $ length s
    t = zip s [0..]
    pos = fromJust . flip lookup t
    f = foldr (\ x y -> pos x + y * l) 0

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
    t = zip s [1..]
    pos = fromJust . flip lookup t
    f = foldr (\ x y -> pos x + y * l) 0

-- | List of symbols for binary encoding.
-- For use with 'encodeBase' and 'decodeBase'.
binBase :: [Char]
binBase = "01"

-- | List of symbols for octal encoding.
-- For use with 'encodeBase' and 'decodeBase'.
octBase :: [Char]
octBase = "01234567"

-- | List of symbols for decimal encoding.
-- For use with 'encodeBase' and 'decodeBase'.
decBase :: [Char]
decBase = "0123456789"

-- | List of symbols for hexadecimal encoding.
-- For use with 'encodeBase' and 'decodeBase'.
hexBase :: [Char]
hexBase = "0123456789abcdef"
