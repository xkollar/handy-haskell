module Inputs where

import Data.List

inputs :: [Int]
inputs = [1..100] \\ ([59,61,67,71,73,79,83,89,97] ++ [41,43,47,82,86,94] ++ [53])


startwith :: [Int] -> [Int]
startwith s = s ++ (inputs \\ s)
