-- |Time and size limits
module System.SimpleTimeout.Limits
    ( TimeLimit
    , SizeLimit
    , Budget
    , newBudget
    , checkBudget
    , decSizeBudget
    , showTimeout
    ) where

import System.SimpleTimeout (TimeoutHandle, timeoutHandle, timeout)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)

--------------

-- |Time limit is a 'Double' which is the allowed time in seconds.
type TimeLimit = Double

-- |Size limit is an 'Int' which meaning is given by 'checkBudget' and 'decSizeBudget'.
type SizeLimit = Int

-- |A 'Budget' contains a time and size limit.
data Budget 
    = Budget TimeoutHandle (MVar SizeLimit)

-- |Create a new budget.
newBudget :: TimeLimit -> SizeLimit -> IO Budget
newBudget t s = do
    th <- timeoutHandle t
    mv <- newMVar s
    return $ Budget th mv

-- |Check budget and take another action if there is no more resource.
checkBudget 
    :: Budget 
    -> Int                  -- ^ decrement size budget with this value
    -> (Double -> IO a)     -- ^ what to do in case of timeout ('Double': percent when the thread was started)
    -> IO a                 -- ^ what to do in case there is no more space 
    -> IO a                 -- ^ what to do in a normal case
    -> IO a
checkBudget (Budget tb sb) dec ta sa na = do
    r <- modifyMVar sb $ \a -> return $ 
        if a > 0 then (a-dec, True) else (a, False)
    if r then timeout tb ta na else sa

-- |Decrement free size in a budget.
decSizeBudget 
    :: Budget
    -> (SizeLimit -> (SizeLimit, a))    -- ^ funtion to modify free size and produce a value
    -> IO a
decSizeBudget (Budget _ sb) f
    = modifyMVar sb $ return . f


showTimeout :: Double -> String
showTimeout d  
    = "timeout at " ++ show (round $ 100 * d :: Int) ++ "%" 


