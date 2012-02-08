{-# LANGUAGE DeriveDataTypeable #-}
-- |Intended for internal use: Simple timeout mechanism
module System.SimpleTimeout
    ( TimeoutHandle
    , timeoutHandle
    , timeout
    ) where

import Control.Exception (Exception, handle)
import Control.Concurrent (forkIO, threadDelay, throwTo, ThreadId, myThreadId)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, takeMVar, putMVar, swapMVar, modifyMVar)

import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Typeable (Typeable)

------------------------

-- |timeout exception
--
-- The @Double@ parameter documented at 'timeout'.
data TimeOutException  
    = TimeOutException Double
        deriving (Eq, Typeable)

instance Show TimeOutException where
    show (TimeOutException d) = "<<timeout at " ++ show (round $ 100*d :: Integer) ++ "%>>"

instance Exception TimeOutException

---------------

-- |Abstract data structure used by 'TimeoutHandle' and 'timeout'.
newtype TimeoutHandle 
    = TimeutHandle (MVar 
        (Maybe [(ThreadId, UTCTime)]))
            -- ^ 
            -- @Nothing@: the timeout happened already
            -- @Just xs@: there is time left
            --   @xs@ contains the list of threads for which a 'TimeoutException' 
            --         will be thrown when the time is over.
            --   'UTCTime' is needed to compute the @Double@ parameter of the exception.

-- |Creates a 'TimeoutHandle'.
--
-- The @Double@ parameter is the time limit in seconds.
-- All operations behind 'timeout' will be stopped 
-- at the current time plus the time limit.
timeoutHandle :: Double -> IO TimeoutHandle
timeoutHandle limit = do
    th <- newMVar $ Just []
    _ <- forkIO $ killLater th
    return $ TimeutHandle th
  where

    killLater th = do
        start <- getCurrentTime
        threadDelay $ round $ 1000000 * limit
        Just threads <- swapMVar th Nothing

        end <- getCurrentTime
        let whole = end `diffUTCTime` start

        let kill (x, time) 
                = x `throwTo` 
                    TimeOutException (realToFrac $ (time `diffUTCTime` start) / whole)

        mapM_ kill threads


-- | Stop an operation at a time given by 'timeoutHandle'.
--
-- The @Double@ parameter is a percent between 0 and 1.
-- 
--  * 0: 'timeout' was called right after the 'TimeoutHandle' was created.
--
--  * 1: 'timeout' was called after the time of the timeout.
--
--  * near to 1: 'timeout' was called right before the time of the timeout.
--
--  * Other values: proportional to the time spend by the operation.
timeout 
    :: TimeoutHandle    -- ^ knows the time of the timeout and the creation time of itself
    -> (Double -> IO a) -- ^ timeout handling action for which will the percent will be supplied
    -> IO a             -- ^ the operation to timeout
    -> IO a
timeout (TimeutHandle th) handleTimeout operation = do
    result <- newEmptyMVar

    let handleTimeoutException (TimeOutException d) 
            = handleTimeout d >>= putMVar result

    _ <- forkIO $ handle handleTimeoutException $ do
        b <- modifyMVar th $ \b -> case b of
            Nothing -> return (Nothing, False)
            Just xs -> do
                pid <- myThreadId
                time <- getCurrentTime
                return (Just $ (pid,time):xs, True)
        x <- if b 
            then operation
            else handleTimeout 1
        putMVar result x

    takeMVar result



