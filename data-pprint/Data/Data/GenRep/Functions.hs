-- |Intended for internal use: Generic representation of 'Data' vales.
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Data.GenRep.Functions
    ( mistify
    , numberErrors
    , getErrorIndex
    ) where

import Data.Data.GenRep.Doc (toDoc)
import Data.Data.GenRep
import System.SimpleTimeout.Limits (showTimeout)

import Control.Monad.State (State, runState, get, put)

---------------------------------------

-- | True for 'Hole', 'ListHole' and 'Detail' constructors.
-- Used in 'mistify'.
isJoker :: GenericData -> Bool
isJoker Hole        = True
isJoker ListHole    = True
isJoker (Detail _)  = True
isJoker _ = False

-- could be better
-- | Try to hide some part of the value.
--
-- This is used in the evaluation of exercises, when the result
-- is wrong. We would like to show the erroneous part but not the whole result.
mistify :: GenericData -> GenericData
mistify (Constructor _ []) = Hole
mistify (Constructor p ss) | not (any isJoker ss) = Constructor p $ map mistify ss
mistify x = x

 
-------------------------------------------------------

-- |Collect and number 'Error' values and replace them
-- by an indexed bottom sign.
-- Repeated errors will get the same number.
numberErrors 
    :: [GenericData]
    -> ([GenericData], [(String, String)])
numberErrors l
    = (res, reverse $ map swap errs)
 where
    swap (a,b) = (b,a)

    (res, (_, errs)) = runState (mapM replace l) (0, [])

    replace :: GenericData -> State (Int, [(String, String)]) GenericData
    replace (Constructor p ss) = do
        ss' <- mapM replace ss
        return $ Constructor p ss'
    replace (Error e) = do
        i <- getErrorIndex e
        return $ Error i
    replace (NestedError e) = do
        e' <- replace e
        i <- getErrorIndex (show $ toDoc e')
        return $ Error i
    replace (Timeout d) = do
        i <- getErrorIndex $ showTimeout d
        return $ Error i
    replace (Detail s) = do
        s' <- replace s
        return $ Detail s'
    replace x = return x

getErrorIndex :: String -> State (Int, [(String, String)]) String
getErrorIndex e = do
    (len, es) <- get
    case lookup e es of
        Just x  -> return x
        Nothing -> do
            let n = len+1
                x = '⊥': map toLowerIndex (show n)
            put (n, (e, x): es)
            return x
  where
    toLowerIndex c = case c of
        '0' -> '₀'
        '1' -> '₁'
        '2' -> '₂'
        '3' -> '₃'
        '4' -> '₄'
        '5' -> '₅'
        '6' -> '₆'
        '7' -> '₇'
        '8' -> '₈'
        '9' -> '₉'
        _   -> error $ "toLowerIndex: " ++ show c

