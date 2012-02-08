-- |Compare two 'Data' value with time and size limit
{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}
module Data.Data.Compare 
    ( Answer (..)
    , showAnswer
    , compareData
    ) where

import Data.Data.GenRep
import System.IO.Parallel (twoParallel, manyParallel)
import Data.Data.Eval (evalWithBudget)
import System.SimpleTimeout.Limits

import Control.DeepSeq (NFData, rnf)
import Data.Data (Data, Typeable, gmapQi, toConstr)
import Data.List (minimumBy)
import Data.Ord (comparing)

------------------------

-- |Answer with possibility
--
--      * 'No': no
--
--      * @'Maybe' d@: maybe with d possibility (0-1, 1 denotes yes)
--
--      * 'Yes': yes
data Answer
    = No | Maybe Double | Yes
        deriving (Eq, Ord, Show, Typeable, Data)

instance NFData Answer where
    rnf x = case x of
        Maybe a ->  rnf a
        _       ->  ()

-- |Show an 'Answer' as an equality operator.
showAnswer :: Answer -> String
showAnswer No          = "=/=" 
showAnswer (Maybe _)   = "=?=" 
showAnswer Yes         = "===" 

-----------------------------

-- |Compare two 'Data' value with time and size limit.
compareData 
    :: (Data a) 
    => TimeLimit    -- ^ time limit for comparison decision
    -> TimeLimit    -- ^ time limit for highlighting the difference
    -> SizeLimit    -- ^ size limit for the output (in characters)
    -> a            -- ^ first value
    -> a            -- ^ second value
    -> IO (Answer, GenericData, GenericData)
compareData t1 t2 s x y = do
    b1 <- newBudget t1 maxBound
    (ans, l) <- decideEquality b1 x y
    b2 <- newBudget t2 s
    (x, y) <- fmap assemble $ manyParallel $ map (showPart b2) $ collapsePath l
    return (ans, x, y)
 where
    showPart budget (is, (DData x, DData y)) = do
        p <- twoParallel (evalPath budget is x) (evalPath budget is y)
        return (is, p)

data DData = forall a. Data a => DData a

type Path a = [([Int], (a, a))]

collapsePath :: Path DData -> Path DData
collapsePath xs = case splitAt 3 xs of
    (l, []) -> l
    (l, m) -> l ++ case splitAt 3 $ reverse m of
        (m, []) -> reverse m
        (m, k) -> case reverse k of
            (i,p):k -> (i ++ concatMap fst k, p): reverse m
            _       -> error "collapsePath"

assemble :: Path GenericData -> (GenericData, GenericData)
assemble [([], p)] = p
assemble ((is, (x, y)) : xys) = case assemble xys of
    (x', y') -> (g is x x', g is y y')
  where
    g [] _ x' = x'
    g (i:is) (Constructor c l) x' = Constructor c $ rep i l $ \x -> g is x x'
    g (_:is) _ x' = detail $ g is Hole x'

    detail (Detail x) = Detail x
    detail x = Detail x

    rep 0 (x:xs) f = f x: xs
    rep i (x:xs) f = x: rep (i-1) xs f
    rep _ _ _ = error "rep"
assemble _ = error "assemble"


evalPath :: Data a => Budget -> [Int] -> a -> IO GenericData
evalPath budget [] x 
    = evalWithBudget budget x
evalPath budget (j:is) x 
    = constructor budget x
    $ manyParallel 
      [ gmapQi i (if i==j then evalPath budget is else evalWithBudget budget) x
      | i <- [0..arity x - 1]]


decideEquality :: (Data a, Data b) => Budget -> a -> b -> IO (Answer, Path DData)
decideEquality budget x y = do
    a <- observe x
    b <- observe y
    a `f` b
  where
    observe x = fmap simplify $ constructor budget x $ return []
      where
        simplify (NestedError _) = Error "undefined error"
        simplify Hole  = Timeout 1
        simplify other = other

    Error _   `f` Error _    = yes
    Timeout b `f` Timeout b' = may $ 1 - abs (b - b')
    Timeout b `f` Error _    = may $ 1 - b
    Error _   `f` Timeout b  = may $ 1 - b
    Timeout b `f` _          = may b
    _         `f` Timeout b  = may b
    Error _   `f` _          = no
    _         `f` Error _    = no
    _         `f` _         | toConstr x /= toConstr y 
                             = no
    _         `f` _          = do

        r <- manyParallel 
              [ gmapQi i (gmapQi i (decideEquality' i) x) y | i <- [0..arity x - 1]]
        case r of
            [] -> yes
            xs -> return $ minimumBy (comparing fst) xs

    pair = (DData x, DData y)

    yes   = return (Yes, [([], pair)])
    may z = return (Maybe z, [([], pair)])
    no    = stop >> return (No, [([], pair)])
      where
        stop = decSizeBudget budget $ const (-1,())

    decideEquality' i x y = do
        (ans, ps) <- decideEquality budget x y
        return (ans, ([i], pair):ps)

