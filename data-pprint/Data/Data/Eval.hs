-- |Conversion to 'GenericData' with time and space limit.
module Data.Data.Eval 
    ( eval
    , evalWithBudget
    ) where

import Data.Data.GenRep (GenericData, constructor)
import System.IO.Parallel (manyParallel)
import System.SimpleTimeout.Limits

import Data.Data (Data, gmapQ)

-------------------------

-- |Evaluation with time an size limit.
eval :: Data a => TimeLimit -> SizeLimit -> a -> IO GenericData
eval seconds chars x = do
    b <- newBudget seconds chars
    evalWithBudget b x

-- |Gives more control over the resources
evalWithBudget :: Data a => Budget -> a -> IO GenericData
evalWithBudget b x
    = constructor b x
    $ manyParallel
    $ gmapQ (evalWithBudget b) x



