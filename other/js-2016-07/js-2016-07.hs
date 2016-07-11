base :: [Integer]
base = [1..9]

seqs :: (Num a1, Ord a1) => a1 -> [a] -> [[a]]
seqs n [] = if n > 0 then [] else [[]]
seqs n (x:s)
    | n == 0 = [[]]
    | True = map (x:) (seqs (n-1) s) ++ seqs n s

diagNeg :: [[Integer]] -> [[Integer]]
diagNeg = zipWith (zipWith (*)) . (repeat 1:) . iterate (1:) $ (negate 1:repeat 1)

seqsNeg :: (Num a, Ord a) => [Integer] -> a -> [[Integer]]
seqsNeg s n = concatMap (\ t -> diagNeg $ t : map (const t) t) $ seqs n s

positive :: [Integer] -> Bool
positive = all (>0)

negative :: [Integer] -> Bool
negative = any (<0)

hasAbs n = any ((==) n . abs)
has n = any ((==) n)
doesntHaveAbs n = not . hasAbs n
sumIs n = (==) n . sum

magic :: [a -> Bool] -> [a] -> [a]
magic = foldr (.) id . map filter

or' :: [b -> Bool] -> b -> Bool
or' s x = or $ map (flip id x) s

and' :: [b -> Bool] -> b -> Bool
and' s x = and $ map (flip id x) s

requestedSolution = sum [4,6, 2,6,8,9, 4,5,6, 1,6,4,7, 9,7, 1,4,2, 6,8,1,4, 1,5,4, 6,7,4,1, 6,4]

solution =
    [ " 7 8 9 _ _ _ 5 4 6-1" -- -1
    , " 1 2 7 _ 3 4 6-5 8 9" -- -5
    , " 4-9 5 _ 1 3 2 6 7 _" -- -9
    , " 3 1 6-2 4 5 7 8 9 _" -- -2
    , " 2 4 8 9 _ 7-1 _ _ _" -- -1
    , " _ _ _ 1-9 _ 4 3 2 5" -- -9
    , " _ 5 2 7 6 8 9 1-3 4" -- -3
    , " _ 3 1 8 5-2 _ 7 4 6" -- -2
    , " 6 7-3 4 2 1 _ 9 5 8" -- -3
    , "-5 6 4 3 _ _ _ 2 1 7" -- -5
    ]

-- read from bottom to top...
example = magic
    [ positive        -- all numbers are positive
    , doesntHaveAbs 4 -- there is no +-4
    , doesntHaveAbs 5 -- there is no +-5
    , doesntHaveAbs 9 -- there is no +-9
    , has 6           -- there is 6
    , sumIs 19        -- sum is 19
    ]                 -- where
    $ seqsNeg base 5  -- 5 digit sequences of 1..9
