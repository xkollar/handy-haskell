import Control.Arrow
import Data.List
import Data.Tuple

main :: IO ()
main = mapM_ print
    . filter ((==1) . head . primaries 8 . zipWith (+) [1,0,0])
    . filter ((==[2,2,3]) . primaries 7) $ originals

originals :: [[Integer]]
originals = [[x,y,z]|x <- [0..102], y <- [0..102-x], let z = 102 - x - y]

primaries :: Integer -> [Integer] -> [Integer]
primaries c s = map snd
    . sort . zipWith (\ x ((r,o),n) -> (o,n+x)) remainders
    . sortBy (flip compare) . zipWith (first . flip (,)) [1..] . map swap
    $ base
    where
        base = map (\ x -> (c * x) `divMod` total) s
        remainsTotal = c - sum (map fst base)
        remainders = replicate (fromIntegral remainsTotal) 1 ++ repeat 0
        total = sum s
