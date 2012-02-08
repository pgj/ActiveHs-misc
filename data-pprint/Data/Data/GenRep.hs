-- |Intended for internal use: Generic representation of 'Data' vales.
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Data.GenRep 
    ( ConstructorName (..)
    , GenericData (..)
    , constructor
    , arity
    ) where

import System.SimpleTimeout.Limits
import System.IO.Parallel (manyParallel)
import Control.Exception.Pure (catchPureErrors)

import Control.DeepSeq (NFData, rnf)
import qualified Data.Data as D
import Data.Data (gmapQ, Data, Typeable)

---------------------------------------

-- |Name and precedence of constructors.
data ConstructorName
    = Prefix String     -- ^ used also for literals except characters
    | Char Char         -- ^ character literal
    | Infix Int String
    | Infixr Int String
    | Infixl Int String
    | Tuple Int         -- ^ tuple with n elements
    | Cons              -- ^ nonempty list constructor
    | Nil               -- ^ empty list constructor
        deriving (Eq, Show, Typeable)

instance NFData ConstructorName where
    rnf x = case x of
        Prefix s    -> rnf s
        Infix i s   -> rnf i `seq` rnf s
        Infixr i s  -> rnf i `seq` rnf s
        Infixl i s  -> rnf i `seq` rnf s
        Tuple i     -> rnf i
        Char c      -> rnf c
        _ -> ()


-- |Arity of the toplevel constructor.
arity :: Data a => a -> Int
arity = length . gmapQ (const ())

-- | Extract the name and precedence of a 'Data' value.
precedence :: D.Data a => a -> SizeLimit -> (SizeLimit, ConstructorName)
precedence x v = case D.constrRep c of
    D.CharConstr char  -> (v-1, Char char)
    D.FloatConstr r 
        | t == "Prelude.Double" -> prefix (realToFrac r :: Double)
        | t == "Prelude.Float"  -> prefix (realToFrac r :: Float)
        | otherwise             -> prefix (realToFrac r :: Rational)
    D.IntConstr i   -> prefix i
    D.AlgConstr _   -> case n of
        "[]"  -> (v-2, Nil)
        "(:)" -> (v-1, Cons)
        '(':l | all (==',') (init l) && last l == ')' 
              -> (v-length l-1, Tuple (length l))
        _     -> case D.constrFixity c of
            D.Prefix    -> (v - length n, Prefix n)
            D.Infix     -> (v - length n, Infix 9 n)         -- sorry we can't do better
  where
    prefix :: Show a => a -> (Int, ConstructorName)
    prefix a = (v', Prefix s') where (v', s') = limitString v (show a)

    c = D.toConstr x
    n = D.showConstr c
    t = D.dataTypeName $ D.constrType c


-- |Limit the length of a string by replacing the middle of
-- the string by an ellipsis.
-- The function returns the limit reduced by the final length of the string.
limitString :: SizeLimit -> String -> (SizeLimit, String)
limitString v s = f $ case splitAt i s of
    (_, []) -> s
    (a, b) -> case splitAt (j+1) $ reverse b of
        (_, []) -> s
        (c, _)  -> a ++ "…" ++ reverse (take j c)
 where
    f ss = (v-length ss, ss)

    i = max 4 (v `div` 2)
    j = max 3 (i-2)


---------------------------------

-- |Representation of 'Data' values.
data GenericData
    = Constructor ConstructorName [GenericData]
    | Error String          -- ^ exception error message
    | NestedError GenericData  -- ^ error message which may contain further errors
    | Timeout Double        -- ^ timeout, the @Double@ is between 0 and 1. 
                            -- 
                            --      * 0: evaluation of subexpression started at the beginning
                            -- 
                            --      * towards 1: evaluation of subexpression started near the end of time limit
                            -- 
                            --      * 1: evaluation of subexpression started after time limit (rare)
    | Hole                  -- ^ this is caused space shortage, shown as three dots
    | Detail GenericData    -- ^ also caused by space shortage but this omission a relevant part
    | ListHole              -- ^ used during show
        deriving (Show, Typeable)


instance NFData GenericData where
    rnf x = case x of
        Constructor p s   -> rnf p `seq` rnf s
        Error e         -> rnf e
        NestedError e   -> rnf e
        Detail s        -> rnf s
        Timeout d       -> rnf d
        _   -> ()


-- |Convert a 'Data' value to 'GenericData' given the
-- 'GenericData' representations of the value's children.
constructor :: Data a => Budget -> a -> IO [GenericData] -> IO GenericData
constructor b x m = do
    y <- checkBudget b 1 (return . Left . Timeout) (return $ Left Hole) 
            $ fmap Right $ catchPureErrors x
    case y of
        Left x -> return x
        Right (Left x) -> do 
            fmap NestedError $ evalWithBudget b x
        Right (Right x) -> do 
            p <- decSizeBudget b (precedence x)
            fmap (Constructor p) m 

evalWithBudget :: Data a => Budget -> a -> IO GenericData
evalWithBudget b x
    = constructor b x
    $ manyParallel
    $ gmapQ (evalWithBudget b) x


