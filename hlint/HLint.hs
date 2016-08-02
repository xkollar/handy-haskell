import "hint" HLint.Builtin.All
import "hint" HLint.Default
import "hint" HLint.Dollar

import Data.Map
import Control.Arrow
import Control.Monad
import Control.Monad.State


ignore "Use section"

warn = f . g $ x ==> f $ g x where
    -- https://github.com/ndmitchell/hlint/blob/master/src/HSE/Util.hs
    -- _ = not (isInfixApp x || isApp x || isDo x || isLambda x)
    _ = isAtom x
    note = "unnecessary dot"
warn = (\ x -> f $ g x) ==> f . g

-- I can't write good rule for this...
-- warn = (\ x -> f (g x)) ==> f . g where
--     _ = not (isSection f || isApp (f (g x)))

error "Do not use undefined" = undefined ==> _typed_hole where
    note = "typed holes were introduced with GHC 7.8.1"

-- List
warn = cycle [c] ==> repeat c

warn "Use null" = x == [] ==> null x
warn "Use null" = [] == x ==> null x

warn "Use null" = x /= [] ==> not (null x)
warn "Use null" = [] /= x ==> not (null x)

warn "Use null" = length x > 0 ==> not (null x) where note = "increases laziness"
warn "Use null" = length x >= 1 ==> not (null x) where note = "increases laziness"

warn = fst (unzip x) ==> map fst x
warn = snd (unzip x) ==> map snd x

warn = or (fmap f s) ==> any f s
warn = and (fmap f s) ==> all f s

-- Map
warn = map fst (Data.Map.toList x) ==> Data.Map.keys x
warn = map snd (Data.Map.toList x) ==> Data.Map.elems x

warn = foldr f v (Data.Map.elems x) ==> Data.Map.foldr f v x
warn = Data.Maybe.fromMaybe d (Data.Map.lookup k m) ==> Data.Map.findWithDefault d k m

-- Arrows
warn = id *** id ==> id where note = "generalize"
warn = Control.Arrow.first id ==> id where note = "generalize"
warn = Control.Arrow.second id ==> id where note = "generalize"

warn = Control.Arrow.first f (Control.Arrow.second g x) ==> (f Control.Arrow.*** g) x
warn = Control.Arrow.second f (Control.Arrow.first g x) ==> (g Control.Arrow.*** f) x

warn = Control.Arrow.first f (Control.Arrow.first g x) ==> Control.Arrow.first (f . g) x
warn = Control.Arrow.second f (Control.Arrow.second g x) ==> Control.Arrow.second (f . g) x

warn = (a *** b) ((c &&& d) x) ==> ((a . c) Control.Arrow.&&& (b . d)) x
warn = (a *** b) ((c *** d) x) ==> ((a . c) Control.Arrow.*** (b . d)) x

warn = Control.Arrow.first f ((a *** b) x) ==> ((f . a) Control.Arrow.*** b) x
warn = Control.Arrow.second f ((a *** b) x) ==> (a Control.Arrow.*** (f . b)) x
warn = (a *** b) (Control.Arrow.first f x) ==> ((a . f) Control.Arrow.*** b) x
warn = (a *** b) (Control.Arrow.second f x) ==> (a Control.Arrow.*** (b . f)) x

warn = Control.Arrow.first f ((a &&& b) x) ==> ((f . a) Control.Arrow.&&& b) x
warn = Control.Arrow.second f ((a &&& b) x) ==> (a Control.Arrow.&&& (f . b)) x

warn = fst &&& snd ==> id where note = "generalize"
warn = snd &&& fst ==> Data.Tuple.swap

-- State
warn = fmap f Control.Monad.State.get ==> Control.Monad.State.gets f
warn = Control.Monad.liftM f Control.Monad.State.get ==> Control.Monad.State.gets f
