newtype ModN a = ModN { evalModN :: a -> a }

type Op a = a -> a -> a

modOp :: Integral a => Op a -> Op (ModN a)
modOp op a b = ModN $ \ n -> (evalModN a n `op` evalModN b n) `mod` n

app :: Integral a => (a -> a) -> ModN a -> ModN a
app f m = ModN $ \ n -> f (evalModN m n) `mod` n

instance (Integral a, Num a) => Num (ModN a) where
    fromInteger m = ModN $ \ n -> fromInteger m `mod` n
    (+) = modOp (+)
    (*) = modOp (*)
    (-) = modOp (-)
    negate = app negate
    abs = id
    signum = app signum
