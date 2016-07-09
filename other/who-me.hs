import Control.Arrow
import Data.Char
import Data.List
import Data.Maybe
import Data.Tuple
import Handy

-- https://www.youtube.com/watch?v=Ea7dbysVLP0&feature=youtu.be
-- poster: The Vernissage
-- title: 119 104 111 044 032 109 101 063
--
input = "055 055 056 056 056 040 056 056 056 056 055 040 040 040 040 040 055\
    \ 056 056 056 056 040 055 055 055 056 056 040 040 040 040 040 055 056 056\
    \ 056 056 040 056 056 056 056 056 040 040 040 040 040 056 056 055 055 055\
    \ 040 055 055 055 055 055 040 040 040 040 040 055 056 056 056 056 040 055\
    \ 056 056 040 040 040 040 040 055 055 056 056 056 040 055 055 055 055 056\
    \ 040 040 040 040 040 055 055 056 056 056 040 056 056 056 055 055 040 040\
    \ 040 040 040 055 055 056 056 056 040 056 056 056 056 055 040 040 040 040\
    \ 040 055 056 056 056 056 040 056 056 056 056 056 040 040 040 040 040 055\
    \ 055 056 056 056 040 056 056 055 055 055 040 040 040 040 040 055 055 056\
    \ 056 056 040 055 055 055 055 056 040 040 040 040 040 056 056 055 055 055\
    \ 040 055 055 055 055 055 040 040 040 040 040 055 056 056 056 056 040 056\
    \ 056 055 055 055 040 040 040 040 040 055 056 056 056 056 040 056 056 056\
    \ 056 056 040 040 040 040 040 055 056 056 056 056 040 055 055 056 056 056\
    \ 040 040 040 040 040 055 056 056 056 056 040 055 055 055 055 056 040 040\
    \ 040 040 040 055 056 056 056 056 040 056 040 040 040 040 040 055 055 056\
    \ 056 056 040 056 056 056 055 055"

-- title = "who, me?"
title = map (chr . decodeBase decBase) $ words "119 104 111 044 032 109 101 063"

splitOn' :: ([a] -> Bool) -> [a] -> ([a], [a])
splitOn' p = f [] where
    f a [] = (reverse a,[])
    f a xs@(x:s) = if p xs then (reverse a,xs) else f (x:a) s

unfold :: (a -> (b,a)) -> (a -> Bool) -> a -> [b]
unfold f p = rec where
    rec x = if p x then [] else h : rec t where
        (h,t) = f x

sepBySeq :: Eq a => [a] -> [a] -> [[a]]
sepBySeq x = unfold (second (drop n) . splitOn' (isPrefixOf x)) null where
    n = length x

translate :: Eq a => [(a,b)] -> b -> a -> b
translate t d x = fromMaybe d $ lookup x t

morse =
    [ ('A', ".-")
    , ('B', "-...")
    , ('C', "-.-.")
    , ('D', "-..")
    , ('E', ".")
    , ('F', "..-.")
    , ('G', "--.")
    , ('H', "....")
    , ('I', "..")
    , ('J', ".---")
    , ('K', "-.-")
    , ('L', ".-..")
    , ('M', "--")
    , ('N', "-.")
    , ('O', "---")
    , ('P', ".--.")
    , ('Q', "--.-")
    , ('R', ".-.")
    , ('S', "...")
    , ('T', "-")
    , ('U', "..-")
    , ('V', "...-")
    , ('W', ".--")
    , ('X', "-..-")
    , ('Y', "-.--")
    , ('Z', "--..")
    , ('0', "-----")
    , ('1', ".----")
    , ('2', "..---")
    , ('3', "...--")
    , ('4', "....-")
    , ('5', ".....")
    , ('6', "-....")
    , ('7', "--...")
    , ('8', "---..")
    , ('9', "----.")
    ]

-- Punctuation Mark Morse
-- Full-stop (period)  .-.-.-
-- Comma  --..--
-- Colon  ---...
-- Question mark (query)  ..--..
-- Apostrophe  .----.
-- Hyphen  -....-
-- Fraction bar  -..-.
-- Brackets (parentheses) -.--.-
-- Quotation marks  .-..-.
-- At sign  .--.-.
-- Equals sign  -...-
-- Error  ........

-- progress = "the pystery begips"
--             the mystery begins
--                 ^           ^
-- refference to scoby doo?
progress = map (
        chr . decodeBase hexBase
        . map (translate (map swap morse) (error ":-/")) . words)
    . sepBySeq "     "
    . map (chr . decodeBase octBase)
    . words $ input
