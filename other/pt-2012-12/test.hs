
import Data.List ( sort )

example :: [[Int]]
example =
    [[1,2,1]
    ,[4,3,4]
    ,[2,1,2]
    ]

transpose :: [[a]] -> [[a]]
transpose = foldr (zipWith (:)) (repeat [])

pp :: Show a => [[a]] -> IO ()
pp = putStr . unlines . map show

f a = g a (transpose a)

g a1 a2 = pp a1 >> pp a2

-- length $ let c = [1..3]; e = [1..4]; o = [1..5] in [[[aa,ab,ac],[ba,bb,bc],[ca,cb,cc]] | aa<-c, ab<-e, ac<-c, ba<-e, bb<-o, bc<-e, ca<-c, cb<-e, cc<-c]

instSize 0 = 0
instSize 1 = 1
instSize n = 3^4 * 4^(4*e) * 5^(e^2) where
    e = max (n-2) 0
