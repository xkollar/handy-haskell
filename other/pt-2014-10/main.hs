
import qualified Data.Set
import System.Environment ( getArgs )

-- data Mod = Mod (Maybe Integer) Integer
--
-- instance Show Mod where
--     show (Mod a n) = "(" ++ show (maybe id (flip mod) a n) ++ " ~/ " ++ maybe "?" show a ++ ")"
--
-- instance Eq Mod where
--     Mod (Just x) n == Mod (Just y) m = x == y && n `mod` x == m `mod` y
--     _ == _ = False
--
-- opLift :: (Integer -> Integer -> Integer) -> Mod -> Mod -> Mod
-- opLift g = f where
--     Mod (Just a) n `f` Mod (Just b) m = if a /= b then error "incompactible" else Mod (Just a) (g n m `mod` a)
--     Mod (Just a) n `f` Mod Nothing m =  Mod (Just a) (g n m `mod` a)
--     Mod Nothing n `f` Mod (Just a) m =  Mod (Just a) (g n m `mod` a)
--     Mod Nothing n `f` Mod Nothing m =  Mod Nothing (g n m)
--
-- instance Num Mod where
--     (+) = opLift (+)
--     (*) = opLift (*)
--     (-) = opLift (-)
--
--     negate = id
--     abs = id
--     signum = const 1
--     fromInteger n = Mod Nothing n

-- let f a b (x:s) = if x `Data.Set.member` a then (Data.Set.size a,x:b, dropWhile (x /=) b) else f (x `Data.Set.insert` a) (x:b) s in id $ f Data.Set.empty [] [ 2^3^n `mod` (10^3) | n <- [1..] ]
-- magic x = id $ f Data.Set.empty [] [ 2^n `mod` (10^x) | n <- [0..] ] where
--     f a b (x:s) = if x `Data.Set.member` a
--         -- then (Data.Set.size a,x:b, dropWhile (x /=) b)
--         then (Data.Set.size a)
--         else f (x `Data.Set.insert` a) (x:b) s

-- fromMod (Mod _ i) = i

{-
*Main> let f a b (x:s) = if x `Data.Set.member` a then (Data.Set.size a,x:b, dropWhile (x /=) b) else f (x `Data.Set.insert` a) (x:b) s in id $ f Data.Set.empty [] [ 2^(3^n `mod` 102 ) `mod` (10^3) | n <- [1..] ]
(16,[8,712,872,528,808,152,568,792,688,592,832,768,888,352,728,512,8],[8])
*Main> let f a b (x:s) = if x `Data.Set.member` a then (Data.Set.size a,x:b, dropWhile (x /=) b) else f (x `Data.Set.insert` a) (x:b) s in id $ f Data.Set.empty [] [ 2^(3^n) `mod` (10^3) | n <- [1..] ]
(20,[8,752,928,112,808,152,128,712,608,552,328,312,408,952,528,912,208,352,728,512,8],[8])
*Main> let f a b (x:s) = if x `Data.Set.member` a then (Data.Set.size a,x:b, dropWhile (x /=) b) else f (x `Data.Set.insert` a) (x:b) s in id $ f Data.Set.empty [] [ 2^((3^n - 2) `mod` 100 + 2 ) `mod` (10^3) | n <- [1..] ]
(20,[8,752,928,112,808,152,128,712,608,552,328,312,408,952,528,912,208,352,728,512,8],[8])
-}
-- (20,[8,752,928,112,808,152,128,712,608,552,328,312,408,952,528,912,208,352,728,512,8],[8])
--                     [8,712,872,528,808,152,568,792,688,592,832,768,888,352,728,512,8],[8])
--   (20,[8,2,928,112,808,152,128,712,608,552,328,312,408,952,528,912,208,352,728,512,8],[8])
--

-- main = let f a b (x:s) = if x `Data.Set.member` a then (Data.Set.size a, length (dropWhile (x /=) b) - 1) else f (x `Data.Set.insert` a) (x:b) s
--        in print $ f Data.Set.empty [] [ 2^((n - 9) `rem` 7812500 + 9) `mod` (10^10) | n <- [1..] ]

findLoop :: Ord a => [a] -> (Data.Set.Set a, a)
findLoop = f Data.Set.empty where
    f a (x:s) = if x `Data.Set.member` a
        then (a, x)
        else f (x `Data.Set.insert` a) s
    f _ _ = error "Non looping (finite)  list"

findLoopParams :: (Ord a) => [a] -> (Int, Int)
findLoopParams s = f $ findLoop s where
    f (a, x) = (m, n) where
        m = length . takeWhile (x /=) $ s
        n = Data.Set.size a - m

opt :: Integer -> Integer -> Integer -> Integer
opt m n x = if x < m then x else (x - m) `mod` n + m

-- ((2^) . opt 4 500 . (3^) . opt 2 100 . (4^)) x `mod` 10^4

magic :: [(Integer, Integer)] -> Integer -> Integer
magic = f 2 where
    f n ((a,b):s) = (n^) . opt a b . f (n+1) s
    f n []    = (n^)

main' :: [String] -> IO ()
main' ["nextParam", n, params] =
    print $ findLoopParams [ magic (read params) x `mod` 10^(read n::Integer) | x <- [0..] ]
main' ["loop", n, params] =
    print $ findLoop [ magic (read params) x `mod` 10^(read n::Integer) | x <- [0..] ]
main' _ =
    putStrLn "Read source (sry)"

main :: IO ()
main = getArgs >>= main'
