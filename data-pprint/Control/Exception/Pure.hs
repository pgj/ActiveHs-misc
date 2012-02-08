-- |Catch exceptions produced in pure code
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Exception.Pure 
    ( catchPureErrors
    , catchPureErrorsSafe
    ) where

import Control.DeepSeq (NFData, deepseq)
import Control.Exception



-- | Evaluate to weak head normal form and catch 
-- exceptions which can be raised by errors in pure computation.
-- See also the "Test.ChasingBottoms.IsBottom" module in ChasingBottoms package.
catchPureErrors :: a -> IO (Either String a)
catchPureErrors a 
    = fmap Right (evaluate a)
      `catches` 
        [ Handler (\(e :: ErrorCall)        -> f e)
        , Handler (\(e :: NonTermination)   -> f e)
        , Handler (\(e :: PatternMatchFail) -> f e)
        , Handler (\(e :: NoMethodError)    -> f e)
        , Handler (\(e :: ArrayException)   -> f e)
        , Handler (\(e :: RecConError)      -> f e)
        , Handler (\(e :: RecSelError)      -> f e)
        , Handler (\(e :: RecUpdError)      -> f e)
        , Handler (\(e :: ArithException)   -> f e)
        , Handler (\(e :: AssertionFailed)  -> f e)
        ]
 where
    f :: Show x => x -> IO (Either String a)
    f = return . Left . show

-- | Make sure that the error message is a concrete String.
catchPureErrorsSafe :: a -> IO (Either String a)
catchPureErrorsSafe a = do
    e <- catchPureErrors a
    case e of
        Right _ -> return e
        Left s -> fmap (either (Left . ("Nested error: "++)) Left) $ catchPureErrorsSafe (s `deepseq` s)




