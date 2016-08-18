{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Timed...
--
-- Timed...
module Handy.Timed (lastTimed) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar
    ( MVar, newEmptyMVar, newMVar, putMVar, readMVar, swapMVar)
import Control.Exception (evaluate)
import Control.Monad (Monad((>>=), return), void)
import Data.Function (($))
import Data.Int (Int)
import System.IO (IO)

worker :: MVar a -> MVar a -> [a] -> IO ()
worker refTemp refFinal = rec
  where
    rec [] = readMVar refTemp >>= putMVar refFinal
    rec (x:s) = do
        x' <- evaluate x
        void $ swapMVar refTemp x'
        rec s

waiter :: Int -> MVar a -> MVar a -> IO ()
waiter t refTemp refFinal = do
    threadDelay t
    readMVar refTemp >>= putMVar refFinal

-- | @lastTimed n a s@ will return last value of
-- @s@ that was produced within @n@ microseconds.
-- If no value was produced (WHNF) within given time,
-- then @a@ is returned.
lastTimed :: Int -> a -> [a] -> IO a
lastTimed t v s = do
    temp <- newMVar v
    fin <- newEmptyMVar

    workerId <- forkIO $ worker temp fin s
    waiterId <- forkIO $ waiter t temp fin
    res <- readMVar fin
    killThread workerId
    killThread waiterId
    return res

