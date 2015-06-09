-- Inspired by IRC bot on HaskellWiki
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad.Reader
import Data.List
import Data.Time
import Network
import Prelude hiding (catch, log)
import System.Environment (getArgs)
import System.IO
import System.IO.Error hiding (catch)
import Text.Printf
-- import Data.Time.ISO8601 (formatISO8601Millis)


import IRCParser (parseMessage)
import IRCTypes
import FormatTime

data Config = Config
    { server :: String
    , port :: Int
    , nick :: String
    , channels :: [String]
    , logLevel :: LogLevel
    , reconnectTime :: Int
    }

data Bot = Bot
    { config :: Config
    , socket :: Handle
    }

type Net = ReaderT Bot IO

data LogLevel = Debug | Normal deriving (Eq, Ord)

data Direction = From | To

main :: IO ()
main = do
    (server' : port' : nick' : channels') <- getArgs
    hSetBuffering stdin NoBuffering
    connectLoop $ Config { server = server', port = read port', nick = nick', channels = channels', logLevel = Normal, reconnectTime = 60 }

connectLoop :: Config -> IO ()
connectLoop c = bracket (connect c) disconnect (runBot c) `catch` handler where
    disconnect = notify "Closing connection" . hClose
    handler e
        | isEOFError e = do
            let seconds = reconnectTime c
            printf "Connection closed. Reconnecting in %d seconds\n" seconds
            threadDelay $ seconds * 1000000
            connectLoop c
        | otherwise = printf "%s (terminating)\n" (show . ioeGetErrorType $ e)

notify :: String -> IO a -> IO a
notify s a = do
    printf "%s ... " s
    hFlush stdout
    x <- a
    putStrLn "done."
    return x

connect :: Config -> IO Handle
connect c = notify note $ do
    h <- connectTo (server c) . PortNumber . fromIntegral $ port c
    hSetEncoding h utf8
    hSetBuffering h NoBuffering
    return h
    where
    note = "Connecting to " ++ server c

runBot :: Config -> Handle -> IO ()
runBot c h = runReaderT run $ Bot { config = c, socket = h }

run :: Net ()
run = do
    n <- asks $ nick . config
    write Normal "NICK" n
    write Normal "USER" (n ++ " 0 * :take n . md5")
    asks (channels . config) >>= mapM_ (write Normal "JOIN")
    write Normal "AWAY" ":Just idling..."
    -- write "JOIN" chan
    asks socket >>= listen

log :: Direction -> String -> Net ()
log d s = do
    t <- io getCurrentTime
    _ <- io $ printf "[%s]%s %s\n" (formatISO8601 t) (dir d) s
    return ()
    where
    dir From = ">"
    dir To   = "<"

--
-- Process each line from the server
--
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    case parseMessage s of
        Right (ServerMessage
            { command = StringCommand "PING"
            , params = [x]
            }) -> write Debug "PONG" x
        Right (ServerMessage
            { command = StringCommand "PRIVMSG"
            , params = [chan,m]
            , prefix = Just u
            }) -> log From (takeWhile ('!'/=) u ++ "@" ++ chan ++ ": " ++ m)
        Right (ServerMessage
            { command = NumericCommand x
            , params = p
            }) -> log From ("#" ++ x ++ ": " ++ last p)
        Right _ -> log From ("###Other: " ++ s)
        Left _ -> log From ("###Unparsable: " ++ s)
    -- log From s
    -- react
    -- when (ping s) (pong s)
    -- where
    -- ping x    = "PING :" `isPrefixOf` x
    -- pong x    = write Debug "PONG" (':' : drop 6 x)

--
-- Send a message out to the server we're currently connected to
--
write :: LogLevel -> String -> String -> Net ()
write l s t = do
    h <- asks socket
    _ <- io $ hPrintf h "%s %s\r\n" s t
    l' <- asks $ logLevel . config
    when (l >= l') . io $ printf    "> %s %s\n" s t

--
-- Convenience functions
--
handleAnyException :: a -> SomeException -> a
handleAnyException h _ = h

io :: IO a -> Net a
io = liftIO
