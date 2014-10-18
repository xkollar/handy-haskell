
import SimpleGraphics
import Data.List
import System

size :: Double
size = 20

sep :: Double
sep = 2

swap = uncurry (flip (,))

anotater f x = (x,f x)
anotatel f x = (f x,x)

square c x = withFill c $ rectangle x x

uniq :: Ord a => [a] -> [a]
uniq (x:s) = x : uniq (dropWhile (x==) s)
uniq [] = []

allEdges :: [(Int,Int)] -> [(Int,Int)]
allEdges s = uniq . sort $ s ++ map swap s

magic :: [(Int, Int)] -> Graphic
magic es = overGraphics $ map (drawSq . anotater f) grid where
    typeExplicit = es :: [(Int,Int)]
    grid = [(x,y)| y <- [ymin..ymax], x <- [xmin..xmax]]
    xmin = minimum xs
    xmax = maximum xs
    ymin = minimum ys
    ymax = maximum ys
    xs = map fst aes
    ys = map snd aes
    aes = allEdges es
    f t@(x,y)
        | x == y                  = Red
        | t `elem` aes && umin t `elem` map umin (filter (t/=) ses) = Magenta
        | t `elem` aes            = Blue
        | umin t `elem` ds        = RGB 200 200 200
        | otherwise               = None
    drawSq ((x,y),c) = translate (mul * fromIntegral x,mul * fromIntegral (ymax - y + 1)) (square c size)
    mul = size+2*sep
    ds = uniq . sort $ map umin ses
    umin = uncurry (-)
    ses = map st es
    st t@(x,y) = (if x <= y then id else swap) t

-- [(7,1),(6,1),(5,1),(5,2),(4,2),(3,2)]
-- [(7,1),(7,2),(7,3),(6,3),(6,4),(6,5)]
-- [(7,1),(7,4),(6,1),(5,1),(4,2),(4,3)]
-- [(7,1),(7,3),(6,1),(5,3),(4,1),(3,2)]

fewEdges :: [(Int,Int)] -> [(Int,Int)]
fewEdges = uniq . sort . map st where
    st t@(x,y) = (if x <= y then id else swap) t

gvproc :: [(Int,Int)] -> String
gvproc s = "graph g { graph [ ranksep=\".2\", nodesep=\".1 equally\" ]; edge [ fontsize=8, fontcolor=\"red\" ]; " ++ concat (map kwak s) ++ "}" where
    kwak (x,y) = show x ++ " -- " ++ show y ++ "[ label = \"" ++ show (abs $ x - y) ++ "\" ]; "

main :: IO ()
main = do
    args <- getArgs
    main' args

main' :: [String] -> IO ()
main' [outfile] = do
    c <- getContents
    let list = read c :: [(Int, Int)]
    writeImage (outfile ++ ".svg") . withStroke Black 1 . magic $ list
    writeFile (outfile ++ ".gv") $ gvproc list
main' _ = error "1 argument (filename without suffix) expected"
