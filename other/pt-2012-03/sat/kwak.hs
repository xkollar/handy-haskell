
import Data.List
import System

data Lit a = Id a | Not a deriving Show
type T = Int
type CNF = [[Lit (T,T,T)]]

level :: T
level = 4

verts :: T
verts = 2^level-1

edges :: T
edges = 2^level-2+1 -- "virtual" edge 0 from node 1 to "parent"

between :: T -> T -> T -> Bool
between l u x = l <= x && x <= u

encode :: (T,T,T) -> T
encode (a,b,c) = if between 1 edges a && between 1 verts b && between 1 verts c then res else error "Encoding error" where
    res = 1 + (b-1) + (c-1) * verts + (a-1) * verts^2

decode :: T -> (T,T,T)
decode x = (a+1,b+1,c+1) where
    x' = x-1
    (x'',b) = x' `divMod` verts
    (a,c) = x'' `divMod` verts

varCount :: T
varCount = edges * verts * verts

varCombs :: [(T,T,T)]
varCombs = [ (p,x,y) | p <- [1..edges], x <- [1..verts], y <- [1..verts] ]

anotatel :: (a -> b) -> a -> (b,a)
anotatel f x = (f x,x)

edge0 :: CNF
edge0 = [ [ Id (1,1,1) ] ]

everyEdgeAL :: CNF
everyEdgeAL = [ [ Id (p,x,y) | x <- [1..verts], y <- [1..verts] ] | p <- [1..edges] ]
-- can everuEdgeAM be 2 * size / verts ?
-- everyEdgeAM :: CNF
-- everyEdgeAM = [ [Not (p,x,y),Not (p,w,z)]
--     | p <- [2..edges]
--     , x <- [1..verts]
--     , w <- [1..verts]
--     , x /= w
--     , y <- [1..verts]
--     , z <- [1..verts]
--     , y /= z
--     ]

everyEdgeAM :: CNF
everyEdgeAM = everyEdgeAM1 ++ everyEdgeAM2

everyEdgeAM1 :: CNF
everyEdgeAM1 = [ [Not (p,x,y),Not (p,x,w)]
    | p <- [1..edges]
    , y <- [1..verts]
    , w <- [1..verts]
    , y /= w
    , x <- [1..verts]
    ]

everyEdgeAM2 :: CNF
everyEdgeAM2 = [ [Not (p,y,x),Not (p,w,x)]
    | p <- [1..edges]
    , y <- [1..verts]
    , w <- [1..verts]
    , y /= w
    , x <- [1..verts]
    ]

everyValAL :: CNF
everyValAL = [ [ Id (p,i,a) | p <- [1..edges], a <- [1..verts] ] | i <- [1..verts] ]
everyValAM :: CNF
everyValAM = [ [Not (p,i,a) ,Not (q,i,b)]
    | i <- [1..verts]
    , p <- [1..edges]
    , q <- [1..edges]
    , p /= q
    , a <- [1..verts]
    , b <- [1..verts]
    , a /= b
    ]

lengthsAll :: CNF
lengthsAll = [ concat [ [Id (p,a,a+d),Id (p,a+d,a)] | a <- [1..verts-d], p <- [2..edges] ] | d <- [1..edges-1] ]

treeCons :: CNF
treeCons = [ [Not (p,w,x),Not (p`div`2,y,z)]
    | p <- [2..edges] -- maybe 4
    , x <- [1..verts]
    , y <- [1..verts]
    , x /= y
    , w <- [1..verts]
    , z <- [1..verts]
    ]

formula :: CNF
formula = concat
    [ edge0
    , everyEdgeAL
    , everyEdgeAM
    , everyValAL
    , everyValAM
    , lengthsAll
    , treeCons
    ]

ppLit :: Lit (T,T,T) -> String
ppLit (Id x) = show (encode x)
ppLit (Not x) = '-': show (encode x)

ppFormula = unlines . map (ppDj . map ppLit) where
    ppDj =  (++ "0") . concatMap (++" ")

prepareX f = "p cnf " ++ show varCount ++ " _\n" ++ ppFormula f

mainB f = readFile f >>= print . map decode . filter (>0). map (read::String -> T) . words . last . lines

mainA f = writeFile f $ prepareX formula

main' :: [String] -> IO ()
main' ["-g",f] = mainA f
main' ["-d",f] = mainB f
main' _ = error "look to source how to call me (-g/-d filename to Generate/Decode filename)"

main :: IO ()
main = do
    args <- getArgs
    main' args
