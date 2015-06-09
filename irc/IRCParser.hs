module IRCParser (parseMessage) where

-- http://www.rfc-editor.org/rfc/rfc2812.txt
import Control.Monad
import IRCTypes
import BaseParser

messageP :: Parser ServerMessage
messageP = do
    pr <- maybeP p
    co <- commandP
    pa <- paramsP
    eof
    return $ ServerMessage pr co pa
    where
        p = do
            _ <- char ':'
            x <- prefixP
            _ <- space
            return x

prefixP :: Parser String
prefixP = servernameP `mplus` (nicknameP >>++ rescue "" (rescue "" (char '!' >>: userP) >>++ (char '@' >>: hostP)))

commandP :: Parser ServerCommand
commandP = letterCommand `mplus` numericCommand where
    letterCommand = fmap StringCommand $ many1 letter
    numericCommand = fmap NumericCommand $ 3 `times` digit

paramsP :: Parser [String]
paramsP = variant1 `mplus` variant2 where
    variant1 = do
        s <- boundedMany 13 (space >> middleP)
        t <- maybeP (space >> char ':' >> trailing)
        case t of
            Nothing -> return s
            Just x -> return $ s ++ [x]
    variant2 = do
        s <- 14 `times` (space >> middleP)
        t <- maybeP (space >> maybeP (char ':') >> trailing)
        case t of
            Nothing -> return s
            Just x -> return $ s ++ [x]

nospcrlfcl :: Parser Char
nospcrlfcl = sat p where
    p = (`notElem` "\x00\x0A\x0D\x20\x3A")

middleP :: Parser String
middleP = nospcrlfcl >>: many (char ':' `mplus` nospcrlfcl)

trailing :: Parser String
trailing = many (char ':' `mplus` char ' ' `mplus` nospcrlfcl)

space :: Parser Char
space = char '\x20'

servernameP :: Parser String
servernameP = hostnameP

hostP :: Parser String
hostP = hostnameP `mplus` hostaddrP

hostnameP :: Parser String
hostnameP = do
    x <- shortnameP
    fmap (concat . (x:)) $ many (char '.' >>: shortnameP)

shortnameP :: Parser String
-- Real world is sometimes more diverse :-/
-- shortnameP = (letter `mplus` digit) >>: many (letter `mplus` digit `mplus` char '-')
shortnameP = (letter `mplus` digit) >>: many (letter `mplus` digit `mplus` char '-' `mplus` char '/')

-- got bored...
hostaddrP :: Parser String
hostaddrP = many1 (hexdigit `mplus` char '.' `mplus` char ':')

nicknameP :: Parser String
-- In reality, usually longer than 8 is allowed
-- nicknameP = (letter `mplus` special) >>: boundedMany 8 (letter `mplus` digit `mplus` special `mplus` char '-')
nicknameP = (letter `mplus` special) >>: many (letter `mplus` digit `mplus` special `mplus` char '-')

letter :: Parser Char
letter = sat p where
    p x = '\x41' <= x && x <= '\x5A' -- A-Z
        || '\x61' <= x && x <= '\x7A' -- a-z

userP :: Parser String
userP = many1 $ sat p where
    p = (`notElem` "\x00\x0A\x0D\x20\x40") -- "\NUL\n\r @"

digit :: Parser Char
digit = sat p where
    p x = '\x30' <= x && x <= '\x39' -- 0-9

hexdigit :: Parser Char
-- Oh cruel real world...
-- hexdigit = digit `mplus` sat (`elem` "ABCDEF")
hexdigit = digit `mplus` sat (`elem` "ABCDEFabcdef")

special :: Parser Char
special = sat p where
    p x = '\x5B' <= x && x <= '\x60'
        || '\x7B' <= x && x <= '\x7D'
        -- "[", "]", "\", "`", "_", "^", "{", "|", "}"

parseMessage :: String -> Either String ServerMessage
parseMessage s = case runParser messageP s of
    [] -> Left ":-("
    (x:_) -> Right $ fst x
