import Data.List
import System.IO

data BinTree a
    = Node a (BinTree a) (BinTree a)
    | Empty
    deriving (Eq,Show)

inorder' :: BinTree a -> [a]
inorder' Empty = []
inorder' (Node v l r) = inorder' l ++ v : inorder' r

-- inorder :: BinTree a -> [a]
-- inorder = flip f [] where
--     f Empty        = id
--     f (Node v l r) = f l . (v:) . f r
-- inorder :: BinTree a -> [a]
-- inorder = flip (treeFold id f) [] where
--     f v l r = l . (v:) . r

walkTree :: (b -> ([a] -> [a]) -> ([a] -> [a]) -> [a] -> [a]) -> BinTree b -> [a]
walkTree f = flip (treeFold id f) []

inorder = walkTree (\ v l r -> l . (v:) . r)

preorder = walkTree (\ v l r -> (v:) . l . r)

postorder = walkTree (\ v l r -> l . r . (v:))

exTree :: BinTree Integer
exTree = fibTree 10 where

treeFold :: a -> (b -> a -> a -> a) -> BinTree b -> a
treeFold v _ Empty = v
treeFold v f (Node x l r) = f x (treeFold v f l) (treeFold v f r)

fibTree :: Integer -> BinTree Integer
fibTree 0 = Empty
fibTree 1 = Node 1 Empty Empty
fibTree n = Node n (fibTree (n-2)) (fibTree (n-1))

infTree :: a -> BinTree a
infTree x = Node x (infTree x) (infTree x)

treeTake :: Int -> BinTree a -> BinTree a
treeTake 0 _            = Empty
treeTake _ Empty        = Empty
treeTake n (Node v l r) = Node v (f l) (f r) where
    f = treeTake $! n-1

treeLabel :: [a] -> BinTree b -> BinTree a
treeLabel s t = fst (f t s) where
    f Empty s         = (Empty,s)
    f (Node _ l r) [] = error "Not enough labels"
    f (Node _ l r) s  = (Node x ll lr,s2) where
        (ll,x:s1) = f l s
        (lr,s2)   = f r s1

treeLabelPre :: [a] -> BinTree b -> BinTree a
treeLabelPre s t = fst (f t s) where
    f Empty s         = (Empty,s)
    f (Node _ l r) [] = error "Not enough labels"
    f (Node _ l r) s  = (Node x ll lr,s3) where
        (x:s1) = s
        (ll,s2) = f l s1
        (lr,s3)   = f r s2

treeLabelPost :: [a] -> BinTree b -> BinTree a
treeLabelPost s t = fst (f t s) where
    f Empty s         = (Empty,s)
    f (Node _ l r) [] = error "Not enough labels"
    f (Node _ l r) s  = (Node x ll lr,s3) where
        (ll,s1) = f l s
        (lr,s2)   = f r s1
        (x:s3) = s2

uniq :: Eq a => [a] -> [a]
uniq (x:s) = x : uniq (dropWhile (x==) s)
uniq [] = []

uniq' :: Eq a => [a] -> [a]
uniq' (x:s) = x : uniq' (filter (x/=) s)
uniq' [] = []

-- undefined, do we never use the root value...
distances = tail . preorder . d undefined where
    d x (Node v l r) = Node (abs $ x - v) (d v l) (d v r)
    d x _ = Empty

edges = tail . preorder . d undefined where
    d x (Node v l r) = Node (x,v) (d v l) (d v r)
    d x _ = Empty

f = length . uniq . sort . distances

-- treeLabel [1..7] . treeTake 3 $ infTree ()

canonicalizeTree :: Ord a => BinTree a -> BinTree a
canonicalizeTree = ct where
    ct (Node v l@(Node vl _ _) r@(Node vr _ _)) = if vl < vr then Node v l' r' else Node v r' l' where
        l' = ct l
        r' = ct r
    ct (Node v Empty r) = Node v (ct r) Empty
    ct (Node v l Empty) = Node v (ct l) Empty
    ct Empty = Empty

-- (\ n -> map postorder . uniq' . map canonicalizeTree . filter (((2^n - 2)==) . f). map (flip treeLabel (treeTake n $ infTree ())) $ permutations [1..2^n - 1]) 3

niceList s = do
    System.IO.hSetBuffering System.IO.stdout System.IO.NoBuffering
    putStr . unlines $ map show s
    putStrLn "Done"

niceList' :: Show a => [a] -> String
niceList' = unlines . map show

swap = uncurry (flip (,))

st t@(x,y) = (if x <= y then id else swap) t

anotatel f x = (f x,x)
anotater f x = (x,f x)

-- niceList . map (map snd) . sort $ map (\ s -> sort . map (anotatel st) . edges . treeLabelPost s . treeTake 4 $ infTree ()) [[13,14,4,5,11,10,12,8,9,2,6,7,3,15,1]
-- readFile "graph/sol-15/sol-15.po.txt" >>= writeFile "graph/sol-15/sol-15.txt" . niceList' . map (map snd) . sort . map (\ s -> sort . map (anotatel st) . edges . treeLabelPost s . treeTake 4 $ infTree ()) . (read :: String -> [[Int]])
-- (\ n -> map postorder . uniq' . map canonicalizeTree . filter (((2^n - 2)==) . f) . map (flip treeLabelPre (treeTake n $ infTree ())) . map (\s -> 1:(2^n-1):2:s) $ permutations [3..2^n - 2]) 4
