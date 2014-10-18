import Data.Tuple
import Data.Function
import Data.List
import Data.Maybe

data X = In | Out | A | B | C | D | E | F | G | H deriving ( Eq , Show , Enum )

a4 =
    [ [(In,A),(B,Out),(C,D)]
    , [(In,A),(C,Out),(B,D)]
    , [(In,A),(D,Out),(B,C)]
    , [(In,B),(C,Out),(A,D)]
    , [(In,B),(D,Out),(A,C)]
    , [(In,C),(D,Out),(A,B)]
    ]

b4 =
    [ [(Out,C),(D,Out),(A,B)]
    , [(Out,B),(D,Out),(A,C)]
    , [(Out,B),(C,Out),(A,D)]
    , [(Out,A),(D,Out),(B,C)]
    , [(Out,A),(C,Out),(B,D)]
    , [(Out,A),(B,Out),(C,D)]
    ]

complete = concatMap (\ t -> [t,swap t]) . concatMap f where
    f (In,x) = [(In,x),(Out,x)]
    f x = [x]

check = f In False `on` complete where
    f Out p s t = p
    f a p s t = f (fromJust . lookup a $ s) (not p) t s

trb A = E
trb B = F
trb C = G
trb D = H
trb Out = Out

-- b8 = map (\ s -> s ++ map (\(a,b)-> (trb a, trb b)) s) b4
b8 = [ s ++ map (\(a,b)-> (trb a, trb b)) t | s <- b4, t <- b4 ]

a8 = [ f s t | s <- a4, t <- a4 ] where
    f s t = map g s ++ map (\(a,b)-> (trb a, trb b)) (filter ((In/=).fst) t) where
        g (x,Out) = (x,trb . fromJust . lookup In $ t)
        g x = x

-- let x = zip [0..] a4; y = zip [0..] b4 in and [ (a == c) == check b d | (a,b) <- x, (c,d) <- y ]

-- let x = zip [0..] a8; y = zip [0..] b8 in and [ (a == c) == check b d | (a,b) <- x, (c,d) <- y ]

main = if let x = zip [0..] a8; y = zip [0..] b8 in and [ (a == c) == check b d | (a,b) <- x, (c,d) <- y ] then
    let i In = "T"; i x = show x; h (x,y) = i x ++ i y; g = concat . intersperse "," . map h . filter ((Out/=).fst) . filter ((Out/=).snd); f (a,b) = g a ++ ":" ++ g b in putStrLn . unlines . take 32 . map f $ zip a8 b8
    else putStrLn "Ouch!"
