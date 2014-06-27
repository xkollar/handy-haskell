module ModularInverse
    ( egcd
    , modInv
    ) where

-- egcd a b = (x,y,g) => a*x + b*y = g && g = gcd a b
egcd a b = (i * g,i * s,i * t) where
    i = signum g
    (g,s,t) = egcd' a b

egcd' :: Integral a => a -> a -> (a, a, a)
egcd' a 0 = (a, 1, 0)
egcd' a b = (g, t, s - d * t) where
    (d, m) = a `quotRem` b
    (g, s, t) = egcd' b m

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
