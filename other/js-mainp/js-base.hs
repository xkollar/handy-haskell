import Data.Char

nApp :: Int -> (a -> a) -> a -> a
nApp n = foldr (.) id . replicate n
