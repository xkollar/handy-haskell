import Control.Arrow

-- 1506009006001500000000
-- = product [2,2,2,2,2,2,2,2,3,5,5,5,5,5,5,5,5,5,7,7,7,7,11,11,11,11,13,13,13,13]
-- = product [2^8,3^1,5^9,7^4,11^4,13^4]

anotater :: (a -> b) -> a -> (a,b)
anotater = (id Control.Arrow.&&&)

check1 = product [2^8,3^1,5^9,7^4,11^4,13^4] == 1506009006001500000000
check2 = product (map (+1) [8,1,9,4,4,4]) == length sixplets

sixplets :: [[Int]]
sixplets =
    [ [p1,p2,p3,p4,p5,p6]
    |  a <- [0..8+1+9+4+4+4]
    , p1 <- [0.. min 8 a]
    , p2 <- [0.. min 1 (a-p1)]
    , p3 <- [0.. min 9 (a-p1-p2)]
    , p4 <- [0.. min 4 (a-p1-p2-p3)]
    , p5 <- [0.. min 4 (a-p1-p2-p3-p4)]
    , let p6 = a-p1-p2-p3-p4-p5
    , p6 <= 4
    ]

triplets :: [[Int]]
triplets =
    [ [x,y,z]
    | a <- [0.. 3+3+3]
    , x <- [0.. min 3 a]
    , y <- [0.. min 3 (a-x)]
    , let z = a-x-y
    , z <= 3
    ]

magic :: [[Int]] -> [[Int]]
magic = f [] where
    f :: [[Int]] -> [[Int]] -> [[Int]]
    f v (x:s) = if null (filter (flip isProj x) v) then x : f (x:v) s else f v s
    f _ [] = []

isProj :: [Int] -> [Int] -> Bool
isProj s t = and (zipWith (<=) s t) && length (filter not (zipWith (==) s t)) == 1

main :: IO ()
main = print . map (product . zipWith (^) [2,3,5,7,11,13] . map fromIntegral)
    . filter (flip isProj [8,1,9,4,4,4]) $ magic sixplets

