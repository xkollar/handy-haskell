module Main (main) where

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Data.Time
-- import Network
-- import System.IO

newtype From a = From (TChan a)

readFrom :: From a -> IO a
readFrom (From c) = atomically (readTChan c)

newtype To a = To (TChan a)

writeTo :: To a -> a -> IO ()
writeTo (To c) x = atomically (writeTChan c x)

mkPipe :: IO (From a, To a)
mkPipe = fmap (From &&& To) newTChanIO

---------------------------------------------------

data MyState = MyState
    { lastMsgTime :: MVar (Maybe UTCTime)
    }

data MyRead = MyRead
    { sendChan :: To String
    }

type Net = StateT MyState (ReaderT MyRead IO)

runNet :: Net a -> MyState -> MyRead -> IO (a,MyState)
runNet n s r = runReaderT (runStateT n s) r

sender :: From String -> IO ()
sender p = forever $ readFrom p >>= putStrLn

pinger :: To String -> IO ()
pinger p = forever $ do
    writeTo p "PING OMG"
    threadDelay 1000000

mitDaThread :: IO () -> IO c -> IO c
mitDaThread t m = bracket (forkIO t) killThread $ \ x -> m

main :: IO ()
main = do
    (f, t) <- mkPipe
    mitDaThread (sender f) . mitDaThread (pinger t) $ do
        l <- newMVar Nothing
        let myRead = MyRead { sendChan = t }
            myState = MyState { lastMsgTime = l }
        void $ runNet (forever young) myState myRead
        return ()
        where
        young :: Net ()
        young = do
            liftIO $ threadDelay 1300000
            t <- liftIO myThreadId
            pipe <- asks sendChan
            liftIO $ writeTo pipe ("..." ++ show t)
