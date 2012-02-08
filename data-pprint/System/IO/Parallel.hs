-- |Intended for internal use: Parallel evaluation of @IO@ values
module System.IO.Parallel
    ( twoParallel
    , threeParallel
    , fourParallel
    , manyParallel
    ) where

import Control.Concurrent (forkIO, yield)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)


-------------------

-- |Run an @IO@ computation in parallel. The result will appear in the @MVar@.
async :: IO a -> IO (MVar a)
async m = do
    v <- newEmptyMVar
    _ <- forkIO $ do
        x <- m
        yield
        putMVar v x
    return v

-- |Run two @IO@ computations in parallel and wait for the results.
twoParallel :: IO a -> IO b -> IO (a, b)
twoParallel a b = do
    a' <- async a
    b' <- async b
    a'' <- takeMVar a'
    b'' <- takeMVar b'
    return (a'', b'')

-- |Run three @IO@ computations in parallel and wait for the results.
threeParallel :: IO a -> IO b -> IO c -> IO (a, b, c)
threeParallel a b c = do
    a' <- async a
    b' <- async b
    c' <- async c
    a'' <- takeMVar a'
    b'' <- takeMVar b'
    c'' <- takeMVar c'
    return (a'', b'', c'')

-- |Run four @IO@ computations in parallel and wait for the results.
fourParallel :: IO a -> IO b -> IO c -> IO d -> IO (a, b, c, d)
fourParallel a b c d = do
    a' <- async a
    b' <- async b
    c' <- async c
    d' <- async d
    a'' <- takeMVar a'
    b'' <- takeMVar b'
    c'' <- takeMVar c'
    d'' <- takeMVar d'
    return (a'', b'', c'', d'')

-- |Run computations in parallel and wait for the results.
manyParallel :: [IO a] -> IO [a]
manyParallel m 
    = mapM async m >>= mapM takeMVar


