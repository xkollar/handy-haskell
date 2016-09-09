module Graph where

import Control.Arrow
import Data.Function
import Data.List

import qualified Data.Map.Strict as M
import qualified Data.Set as S
-- import qualified Data.Graph as G

-- Graph with nodes v, and edges labelled by e
type Graph v e = M.Map v (M.Map v e)

-- type MyGraph = Graph (Int,Int) Rational

fromList :: (Ord v, Ord e) => [(v,v,e)] -> Graph v e
fromList = foldr f M.empty where
    -- :: a -> b -> b
    f :: (Ord v, Ord e) => (v,v,e) -> Graph v e -> Graph v e
    f (v1,v2,e) = M.insertWith g v1 (M.singleton v2 e)
        where
        g = M.unionWith min

vertices :: (Ord v, Ord e) => Graph v e -> S.Set v
vertices gr = foldr (S.union . M.keysSet) (M.keysSet gr) $ M.elems gr

fsp :: (Num e, Ord e, Ord v) => v -> v -> Graph v e -> (e, [v])
fsp s e gr' = second reverse . M.findWithDefault (0,[]) e
    $ f gr' M.empty (M.singleton s (0, [s]))
    where
    f gr g o = if M.null o
        then g
        else f gr newGreen (orangeCandidate `M.difference` newGreen)
        where
        sel@(v,(e,vs)) = minimumBy (compare `on` second length . snd)
            $ M.toList o
        newGreen = uncurry M.insert sel g
        orangeCandidate = M.unionWith h o
            $ M.mapWithKey
                (\ nv ne -> (ne+e, nv:vs))
                (M.findWithDefault M.empty v gr)
            where
            h x1@(e1,_) x2@(e2,_) = if e1 <= e2 then x1 else x2


data Vrt = A | B | C | D | E | F
  deriving (Eq, Ord, Show)

graphData =
    [ (A,B,4)
    , (A,C,2)
    , (B,C,5)
    , (B,D,10)
    , (C,E,3)
    , (D,F,11)
    , (E,D,4)
    ]

