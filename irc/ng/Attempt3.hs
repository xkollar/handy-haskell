module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import Network (PortID, HostName)
import qualified Network as N
import System.IO
import System.IO.Error

type Lock = MVar ()

data AutoReconnectHandle = H HostName PortID (Handle -> IO ()) Lock (IORef Handle)

connectTo' :: HostName -> PortID -> (Handle -> IO ()) -> IO AutoReconnectHandle
connectTo' host port f = do
    h <- N.connectTo host port
    f h
    l <- newMVar ()
    fmap (H host port f l) $ newIORef h

connectTo :: HostName -> PortID -> IO AutoReconnectHandle
connectTo host port = connectTo' host port $ \ h -> do
    hSetEncoding h utf8
    hSetBuffering h NoBuffering

withLock :: Lock -> IO a -> IO a
withLock l f = withMVar l $ const f

withHandle :: AutoReconnectHandle -> (Handle -> IO a) -> IO a
withHandle a@(H ho po f l r) g = do
    b <- withLock l $ do
        h <- readIORef r
        handleJust (\ e -> if ioeGetHandle e == Just h then Just h else Nothing) reconnect (fmap Just $ g h)
    case b of
        Just x -> return x
        Nothing -> withHandle a g
        -- Nothing -> return undefined
    where
    reconnect h = do
        hClose h
        threadDelay 5000000
        h' <- N.connectTo ho po
        f h'
        writeIORef r h'
        return Nothing

close :: AutoReconnectHandle -> IO ()
close a = withHandle a hClose

sender :: AutoReconnectHandle -> Int -> String -> IO ()
sender a n s = forever $ do
    threadDelay n
    withHandle a $ flip hPutStrLn s

withThread :: IO () -> IO c -> IO c
withThread t m = bracket (forkIO t) killThread $ const m

main' :: AutoReconnectHandle -> IO ()
main' a = withThread (sender a 1000000 "Kwak 1") .
    withThread (sender a 1200000 "Kwak 2") .
    void . forever $ threadDelay 5000000

main :: IO ()
main = bracket connect close main' where

connect :: IO AutoReconnectHandle
connect = connectTo "localhost" (N.PortNumber 8000)
