import Prelude hiding ( gcd )

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

-- | Encode natural number in base `(length s)`.
--
-- > encode ['0'..'9'] 100
-- "100"
--
-- > encode "01" 5
-- "101"
--
-- It is not a bijection due to "leading zeros problem".
encodeBase :: Integral a => [b] -> a -> [b]
encodeBase s = reverse . f where
    l = fromIntegral $ length s
    f 0 = []
    f n = s !! fromIntegral m : f d where
        (d,m) = divMod n l

-- | Bijectively encode number into sequence.
-- This is not standard math encoding, but is handy
-- when you need to use space as efficiently as you can.
--
-- Handy for generating examples: map (encode "aA") [1..]
encodeBij :: Integral a => [b] -> a -> [b]
encodeBij s = reverse . f where
    l = fromIntegral $ length s
    f 0 = []
    f n = s !! fromIntegral m : f d where
        (d,m) = divMod (n - 1) l

