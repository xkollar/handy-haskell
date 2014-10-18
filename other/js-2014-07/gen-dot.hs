
import Data.List
import Inputs

showEdge st va vb = show va ++ " -- " ++ show vb ++ " [" ++ st ++ "];"

showEdges st = unlines . map (uncurry $ showEdge st)

drawPath s = showEdges "color = black" $ zip s (tail s)

checkSol :: [Int] -> Bool
checkSol x = nub (sort x) == sort x && checkSol' x

checkSol' :: [Int] -> Bool
checkSol' s = and $ zipWith check s (tail s)

check :: Int -> Int -> Bool
check x y = x `mod` y == 0 || y `mod` x == 0

drawRemains = showEdges "color=gray" . concat . f where
    f [] = []
    f (x:s) = zip (repeat x) (filter (check x) s) : f s

crossEdges s = concatMap (\ x -> zip (repeat x) $ filter (check x) s)

drawCrossEdges s t = showEdges "color=green weight=0" $ crossEdges s t

drawAll ps r = unlines (map drawPath ps ++ [drawRemains r] ++ [drawCrossEdges (concat ps) r])

writeGraph filename ps r = writeFile filename content where
    content = unlines
        [ "graph " ++ show filename ++ "{"
        , drawAll ps r
        , "}"
        ]

-- (\s -> writeGraph "graph-x.dot" [s] (inputs \\ s)) [100,20,40,80,16,64,32,96,48,24,72,36,18,54,27,81,9,99,33,66,22,44,88,8,56,28,84,12,60,30,90,45,15,75,25,50,10,70,35,7,77,11,55,5,85,17,51,1,58,29,87,3,93,31,62,2,74,37]
