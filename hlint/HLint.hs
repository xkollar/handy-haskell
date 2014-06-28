import "hint" HLint.Builtin.All
import "hint" HLint.Default
import "hint" HLint.Dollar

import Data.Map

warn = (\ x -> f $ g x) ==> f . g
-- List
warn = cycle [c] ==> repeat c

warn = fst (unzip x) ==> map fst x
warn = snd (unzip x) ==> map snd x

warn "Use not . null" = length x > 0 ==> not (null x) where note = "increases laziness"
warn "Use not . null" = length x >= 1 ==> not (null x) where note = "increases laziness"

-- Map
warn = map fst (Data.Map.toList x) ==> Data.Map.keys x
warn = map snd (Data.Map.toList x) ==> Data.Map.elems x

warn = foldr f v (Data.Map.elems x) ==> Data.Map.foldr f v x
warn = Data.Maybe.fromMaybe d (Data.Map.lookup k m) ==> Data.Map.findWithDefault d k m

