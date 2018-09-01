{-# LANGUAGE BangPatterns #-}
-- http://www.newton.ac.uk/news/20170615/1285068

--    0 = 9 - 9
--    1 = 2 - 1
--    1 = 1^2
--    1 = 1^9
--    2 = 2^1
--    2 = 2*1
--    3 = sqrt(9)
--    6 = sqrt(9)!
--    6 = sqrt(9) + sqrt(9)
--
--    1 = 9 - 9 + 2 – 1
--    2 = (9 / 9) + 2 - 1
--    3 = 9 - sqrt(9) - 2 - 1
--    4 = (9 / 9) + 2 + 1
--    5 = sqrt(3) + sqrt(3) - (1^2)
--    6 = (~3)!
--    7 = sqrt(9) + sqrt(9) + 2 - 1
--    8 = sqrt(9*9) - 2 + 1
--    9 = (9+9)/2 * 1
--   10 = sqrt(9*9) + 2 - 1
--   11 = sqrt(9*9) + (2 * 1)
--   12 = sqrt(9*9) + 2 + 1
--   13 = sqrt 9 + 9 + 1^2
--   14 = sqrt 9 + 9 + 1*2
--   15 = 9+9 -2 -1
--   16 = 9+9 - 2^1
--   17 = 9 * 2 - 1^9
--   18 = (9+9) * 1^2
--   19 = 9 * 2 + 1^9
--   20 = (9+(1^9))*2
--   21 = 9 + 9 + 1 + 2
--
--   24 = (~4)!
--
--   27 = sqrt (9*9) * (1+2)
--
--   36 = (9+9) * 2*1
--
--   54 = (9+9) * (2+1)
--
--   56 = (9-2) * (9 - 1)
--
--   62 = (9-2) * 9 - 1
--   63 = (9-2) * 9 ^ 1
--   64 = (9-2) * 9 + 1
--
--   72 = fact 9 `div` fact (9-2) * 1
--
--   81 = 9 * 9 * 1^2
--   82 = 1^2 + 9 * 9
--
--   99 = (9^1) + (9^2)
--  100 = 99 + 2 - 1


-- The Isaac Newton Institute's 25th anniversary puzzle

-- Can you make all of the numbers from 1 up to 100 using, for each sum, all
-- four of the digits in 1992? You must use each digit precisely once but you
-- can use them in any order. You may also use a combination of any of the
-- following operators: addition, subtraction, multiplication, division,
-- parentheses and concatenation (e.g. “29”), square root, factorial (!) and
-- exponentiation.

import Control.Applicative
import Data.Char (isDigit, isSpace)
import Data.List (sort)
import Data.Monoid ((<>))
import Text.ParserCombinators.ReadP as P

import Data.Fix

import Handy.Number (sqrtBig)

-- data Op a
--     = Add a a
--     | Sub a a
--     | Mul a a
--     | Div a a
--     | Exp a a
--     | Sqrt a
--     | Fact a
--     | Val Integer
--   deriving (Eq, Functor, Show)
--
-- type Expr = Fix Op
--
-- data Compose f g a = Compose (f (g a))
--
-- eval :: Op Integer -> Integer
-- eval = \case
--     Add x y -> x + y
--     Sub x y -> x - y
--     Mul x y -> x * y
--     Div x y -> x `div` y
--     Exp x y -> x ^ y
--     Sqrt x -> round (sqrt $ fromIntegral x)
--     Fact x -> fact x
--     Val x -> x
--
-- kuk :: Op String -> String
-- kuk = \case
--     Add x y -> bin "+" x y
--     Sub x y -> bin "-" x y
--     Mul x y -> bin "*" x y
--     Div x y -> bin "/" x y
--     Exp x y -> bin "/" x y
--     Sqrt x -> "(√" <> x <> ")"
--     Fact x -> "(" <> x <> "!)"
--     Val x -> show x
--   where
--     bin op x y = "(" <> x <> op <> y <> ")"

data Op
    = Add
    | Sub
    | Mul
    | Div
    | Exp
    | Sqrt
    | Fact
    deriving (Eq, Show)

data RPN
    = Num Integer
    | Op Op
    deriving (Eq, Show)

type Err = Either String

numP :: ReadP Integer
-- numP = readS_to_P reads
numP = read <$> munch1 isDigit

opP :: ReadP Op
opP = Add <$ char '+'
    <|> Sub <$ char '-'
    <|> Mul <$ char '*'
    <|> Div <$ char '/'
    <|> Exp <$ char '^'
    <|> Sqrt <$ char 's'
    <|> Fact <$ char '!'

rpnP :: ReadP RPN
rpnP = Num <$> numP <|> Op <$> opP

rpnsP :: ReadP [RPN]
rpnsP = (:) <$> rpnP <*> P.many (munch isSpace *> rpnP) <* eof

readRpns :: String -> Err [RPN]
readRpns s = case readP_to_S rpnsP s of
    [(x,"")] -> Right x
    [(x,_)] -> Left "Unconsumed input"
    [] -> Left "No parse"
    _ -> Left "Undeterministic parse"

evalRPN :: [RPN] -> Err Integer
evalRPN x = f [] x where
    f [n] [] = Right n
    f s [] = Left $ "Expression did not evaluate to a single number: " <> show x <> " ~> " <> show s
    f s (Num n:t) = f (n:s) t
    f s (Op o:t) = runOp o s >>= flip f t

runOp :: (Integral a, Show a) => Op -> [a] -> Err [a]
runOp = f where
    f Add (y:x:s) = Right (x+y:s)
    f Sub (y:x:s) = Right (x-y:s)
    f Mul (y:x:s) = Right (x*y:s)
    f Div (y:x:s) = if m == 0 then Right (d:s) else Left "Nonwhole division" where
        (d,m) = x `divMod` y
    f Exp (y:x:s) = Right (x^y:s)
    f Sqrt (x:s) = do
        r <- maybe (Left "Nonwhole sqrt") Right $ sqrt' x
        Right (r:s)
    f Fact (x:s) = Right (fact x:s)
    f o s = Left $ "Unable to evaluate " <> show o <> " on " <> show s

fact :: Integral a => a -> a
fact n = product [1..n]

sqrt' :: Integral a => a -> Maybe a
sqrt' !n = if a * a == n then Just a else Nothing
  where
    a = sqrtBig n

checkNums :: String -> Bool
checkNums = (== sort "1992") . sort . filter isDigit

checkOne :: Integer -> String -> Err ()
checkOne r s = if checkNums s
    then readRpns s >>= evalRPN >>= f
    else Left $ "Bad number use in equation for " <> show r
  where
    f x = if x == r
        then Right ()
        else Left $ "Err: " <> show s <> " evaluates to " <> show x <> " and not " <> show r

checkAll :: [(Integer, String)] -> Err ()
checkAll = mapM_ $ uncurry checkOne

-- 1 2 9 12 19 21 29 91 92 99
-- 1
-- 2
-- 3 (9s)
-- 6 (9s!)

solutions =
    [ (  1, "92 91 -")
    , (  2, "2 1 9 9 - ^ *")
    , (  3, "1 9 9 2 + - -")
    , (  4, "1 2 + 9 9 / +")
    , (  5, "9 s 9 s + 1 2 ^ -")
    , (  6, "9 9 * s 1 - 2 -")
    , (  7, "9 s 9 s + 1 2 ^ +")
    , (  8, "9 9 * s 1 + 2 -")
    , (  9, "9 9 + 2 / 1 *")
    , ( 10, "9 9 * s 2 + 1 -")
    , ( 11, "9 9 * s 2 + 1 *")
    , ( 12, "9 9 * s 2 + 1 +")
    , ( 13, "9 s 9 + 1 2 ^ +")
    , ( 14, "9 s 9 + 1 2 * +")
    , ( 15, "9 9 + 1 - 2 -")
    , ( 16, "9 9 + 2 1 ^ -")
    , ( 17, "9 2 * 1 9 ^ -")
    , ( 18, "9 9 + 1 2 ^ *")
    , ( 19, "9 2 * 1 9 ^ +")
    , ( 20, "9 1 9 ^ + 2 *")
    , ( 21, "9 9 + 1 + 2 +")
    , ( 22, "9 9s+ 1- 2*")
    , ( 23, "92 9 s 1 + /")
    , ( 24, "1 2 + 9 9 / + !")
    , ( 25, "2 9 s 1 + ^ 9 +")
    , ( 26, "9 2 * 9 1 - +")
    , ( 27, "9 9 * s 1 2 + *")
    , ( 28, "9 2 * 9 + 1+")
    , ( 29, "9 1 + 2 * 9 +")
    , ( 30, "19 9 2 + +")
    , ( 31, "91 2+ 9s/")
    , ( 32, "2 9 s 9 s + 1 - ^")
    , ( 33, "99 1 2 + /")
    , ( 34, "9 2 + 9 s * 1 +")
    , ( 35, "9 9 + 2 * 1 -")
    , ( 36, "9 9 + 1 * 2 *")
    , ( 37, "29 9 + 1 -")
    , ( 38, "29 9 + 1 *")
    , ( 39, "29 9 + 1 +")
    , ( 40, "9 s 2 + 9 1 - *")
    , ( 41, "91 9 - 2 /")
    , ( 42, "21 9 s / 9 s ! *")
    , ( 43, "9 9s!1-*2-")
    , ( 44, "19 9s + 2*")
    , ( 45, "1 2 + ! 9 * 9 -")
    , ( 46, "92 9s1-/")
    , ( 47, "19 2 * 9 +")
    , ( 48, "9 1 2^-9s!*")
    , ( 49, "99 1 - 2 /")
    , ( 50, "91 9 + 2 /")
    , ( 51, "9 9s!*2 1+-")
    , ( 52, "9 9s!*2 1^-")
    , ( 53, "9 9s!*1 2^-")
    , ( 54, "9 9 + 1 2 + *")
    , ( 55, "9 1 - 2 ^ 9 -")
    , ( 56, "9 2 - 9 1 - *")
    , ( 57, "19 9s!2/*")
    , ( 58, "9s!9 1+*2-")
    , ( 59, "9s!!9s!/2/1-")
    , ( 60, "9s!!9s!/2/1*")
    , ( 61, "9s!!9s!/2/1+")
    , ( 62, "9 2 - 9 * 1 -")
    , ( 63, "9 2 - 9 1 ^ *")
    , ( 64, "9 2 - 9 * 1 +")
    , ( 65, "2 9 s 9 s + ^ 1 +")
    , ( 66, "9 2+9s!*1*")
    , ( 67, "9 2+9s!*1+")
    , ( 68, "2 9s!^1 9s++")
    , ( 69, "12 9s!* 9s-")
    , ( 70, "9 2 - 9 1 + *")
    , ( 71, "9 2 ^ 9 1 + -")
    , ( 72, "9 ! 9 2 - ! / 1 *")
    , ( 73, "91 2 9 * -")
    , ( 74, "9 2 ^ 9s!-1-")
    , ( 75, "9 9 * 1 2 + ! -")
    , ( 76, "9 2 ^ 9s!-1+")
    , ( 77, "9s!1+9 2+*")
    , ( 78, "99 21 -")
    , ( 79, "9 9* 2-1*")
    , ( 80, "91 2 9 + -")
    , ( 81, "9 9 * 1 2 ^ *")
    , ( 82, "9 9 * 1 2 ^ +")
    , ( 83, "9 9 * 1 2 * +")
    , ( 84, "9 9 * 1 2 + +")
    , ( 85, "91 9s 2*-")
    , ( 86, "91 2 9 s + -")
    , ( 87, "9 9 * 1 2 + ! +")
    , ( 88, "9 1-9 2+*")
    , ( 89, "9 2^ 9 1-+")
    , ( 90, "9 1 ^ 9 2 ^ +")
    , ( 91, "9 1 + 2 ^ 9 -")
    , ( 92, "92 !  91 ! /")
    , ( 93, "99 1 2 + ! -")
    , ( 94, "91 9s!2/+")
    , ( 95, "9s2+19*")
    , ( 96, "99 1 2 + -")
    , ( 97, "99 1 2 * -")
    , ( 98, "99 1 2 ^ -")
    , ( 99, "99 1 2 ^ *")
    , (100, "99 2 + 1 -")
    ]
