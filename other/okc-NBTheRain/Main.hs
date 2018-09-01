import Data.Char
import Data.List.Split
import Handy
import qualified Data.Map as Map
import Data.List
import System.IO

inputTraits = "20266440 22220150 30270160 36237440 22060560 34064556 31271563\
             \ 10064563 10060440 33262544 32267543 34462440 34672141 33462141\
             \ 34462040 31467562 10060440 33264544 31066145 10061554 30271563\
             \ 10062570 32271564 31267143 312270"

inputMedia = "7VQ[UF!,C5FCf>4@Vg1-+EVO7ATVTsEZf(6+DlBHD/!lu/g*`-+DGm>BOPpl+EVNE@q]e!F(\
            \ HJ&+=Cc0G&MMDBlmo6+CT.u+DGF18K_PXA0>T.+EqaHCh+Z-Ec5Dq@Vg<4BOQ'q+EVNE@V$Z\
            \ :0Hb:S+EMHD@;]TuAThX&+EV:.DBO%7AU,DBDfol,+E2@>@UW_^Gp$U1@;]^h+D,Y*EHPi1F\
            \ DQ4T+A!\\lBkq9&D09o6@j#u/Bk(g!BlbD*F`Lo,Cj@.BCh7$rBl7Q+FDi:=ALnsGBPDN1@ps\
            \ 6tG%#E:+Co&&ASu$mDJ()1DBNeA+Dl%8A0>;uA0>u-AKZ&.FEM#6Bl@l38K_GY+CfP7Eb0-1\
            \ Cj@.;DD!&'+CT+0DfB9*+EVNE@;^@4BPD?s+CT.u+Dbb5FCf>4FDi:1+EqO1AKZ/)EbT*,Gp\
            \ %$;+Dl7BBk&b<9lG)pCj@.;FCh\\!*@ps1++A!]\
            \ \"@<?!m+EMI<+EM47GB4m9F`\\a\
            \ 9D^)/g*_.Ch[Zr+D,P1A1eurF"


histogram :: (Ord k, Foldable t) => t k -> Map.Map k Integer
histogram = foldl' (\a v -> Map.insertWith (+) v 1 a) Map.empty

writeBinary :: FilePath -> String -> IO ()
writeBinary fileName = withBinaryFile fileName WriteMode . flip hPutStr
