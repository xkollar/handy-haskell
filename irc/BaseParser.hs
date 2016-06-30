module BaseParser where

import Control.Applicative hiding (many)
import Control.Arrow
import Control.Monad

newtype Parser a = Parser { runParser :: String -> [(a,String)] }

instance Functor Parser where
    fmap f p = Parser $ \ s -> map (first f) (runParser p s)

instance Applicative Parser where
    pure x = Parser $ \ s -> [(x, s)]
    p1 <*> p2 = Parser $ \ s ->
        [ (f x, s2)
        | (f, s1) <- runParser p1 s
        , (x, s2) <- runParser p2 s1
        ]

instance Alternative Parser where
    empty = Parser $ const []
    p1 <|> p2 = Parser $ \ s -> runParser p1 s ++ runParser p2 s

instance Monad Parser where
    return = ret
    p1 >>= f = Parser $ concatMap (uncurry (runParser . f)) . runParser p1

instance MonadPlus Parser where
    mzero = Parser $ const []
    p1 `mplus` p2 = Parser $ \ s -> runParser p1 s ++ runParser p2 s

pass :: Monad m => m a -> m b -> m a
pass x f = x >>= (\ x' -> f >> return x')

(>>:) :: Parser a -> Parser [a] -> Parser [a]
p1 >>: p2 = p1 >>= \ x -> fmap (x:) p2

(>>++) :: Parser [a] -> Parser [a] -> Parser [a]
p1 >>++ p2 = p1 >>= \ x -> fmap (x++) p2

ret :: a -> Parser a
ret x = Parser $ \ s -> [(x,s)]

eof :: Parser ()
eof = Parser f where
    f [] = [((),[])]
    f _ = []

triv :: Parser ()
triv = return ()

sat :: (Char -> Bool) -> Parser Char
sat p = Parser f where
    f [] = []
    f (x:s) = if p x then [(x,s)] else []

any :: Parser Char
any = sat $ const True

char :: Char -> Parser Char
char c = sat (c ==)

many :: Parser a -> Parser [a]
many p = return [] `mplus` many1 p

many1 :: Parser a -> Parser [a]
many1 p = p >>= \ x -> fmap (x:) (many p)

boundedMany :: Int -> Parser a -> Parser [a]
boundedMany 0 _ = return []
boundedMany n p = return [] `mplus` boundedMany1 n p

boundedMany1 :: Int -> Parser a -> Parser [a]
boundedMany1 0 _ = return []
boundedMany1 n p = p >>= \ x -> fmap (x:) (boundedMany (n-1) p)

times :: Int -> Parser a -> Parser [a]
times n p = foldr (\ x y -> x >>= \ a -> fmap (a:) y) (return []) $ replicate n p

maybeP :: Parser a -> Parser (Maybe a)
maybeP p = Parser f where
    f s = case runParser p s of
        [] -> [(Nothing,s)]
        t -> fmap (first Just) t

rescue :: a -> Parser a -> Parser a
rescue x p = Parser f where
    f s = case runParser p s of
        [] -> [(x,s)]
        t -> t

string' :: String -> Parser ()
string' = foldr ((>>) . char) triv

string :: String -> Parser String
string s = const s <$> string' s
