import System

main :: IO ()
main = do
    [fn] <- getArgs
    readFile fn >>= putStrLn . show . filter (uncurry (/=)) . g . map fst . filter ((' '/=) . snd) . f
    where
        f = concat . zipWith (\ y -> zipWith (\ x -> (,) (x,y)) [1..]) [1..] . lines
        g s = map kwak s where
            kwak (x,y) = (x,maxdim - y + 1)
            maxdim = max xmax ymax
            xmax = maximum $ map fst s
            ymax = maximum $ map snd s
