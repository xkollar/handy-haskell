{-# LANGUAGE ScopedTypeVariables #-}
-- Inspired by IRC bot on HaskellWiki
module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Network
import Prelude hiding (catch, log)
import System.IO
import System.IO.Error hiding (catch)
import Text.Printf
import Control.Concurrent.MVar

server, port :: String
server = "127.0.19.1"
port = "9000"

data MyState = MyState
    { lock :: MVar ()
    }

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    connectionLoop

connectionLoop :: IO a
connectionLoop = bracket (connect server $ read port) disconnect runBot `catch` handler where
    disconnect h = notify "Closing connection" $ hClose h
    handler (e::IOError) | isEOFError e = do
        let seconds = 5
        _ <- printf "Connection closed. Reconnecting in %d seconds\n" seconds
        threadDelay $ seconds * 1000000
        connectionLoop
    handler e = printf "Terminating: %s\n" (show . ioeGetErrorType $ e)

pinger h = forever $ do
    threadDelay $ 2 * 1000000
    hPrintf h "PING somemsg...\n"

connect :: String -> Int -> IO Handle
connect server' port' = notify note $ do
    h <- connectTo server' . PortNumber $ fromIntegral port'
    hSetEncoding h utf8
    hSetBuffering h NoBuffering
    forkIO $ pinger h
    return h
    where
    note = "Connecting to " ++ server'

notify :: String -> IO a -> IO a
notify s a = do
    _ <- printf "%s ... " s
    hFlush stdout
    x <- a
    putStrLn "done."
    return x

runBot :: Handle -> IO b
runBot h = do
    _ <- hPrintf h "Omg!\n"
    listen h

listen :: Handle -> IO b
listen h = forever $ do
    s <- hGetLine h
    putStrLn s


test = do
    lock <- newMVar ()
    t <- forkIO $ repeater lock "aaaaaaaaaaaaaaaaaaaaaaaaaaa"
    repeater lock "bbbbbbbbbbbbbbbbbbbbbbbbbbbb" `finally` killThread t
    where
    repeater l x = forever $ do
        withMVar l $ \ _ -> putStrLn x
        threadDelay 1000
