
import Data.List

import Data.Ratio

-- D/O+N/T = P/E+R/I+S/H
f :: [Integer] -> Bool
f [_,0,_,_,_,_,_,_,_,_] = False
f [_,_,_,0,_,_,_,_,_,_] = False
f [_,_,_,_,_,0,_,_,_,_] = False
f [_,_,_,_,_,_,_,0,_,_] = False
f [_,_,_,_,_,_,_,_,_,0] = False
f [d,o,n,t,p,e,r,i,s,h] = d % o + n % t == p % e + r % i + s % h

g [d,o,n,t,p,e,r,i,s,h] = concatMap show [p,o,n,d,e,r] ++ '.' : concatMap show [t,h,i,s]

main = mapM_ (putStrLn . g) . filter f $ permutations [0..9]
