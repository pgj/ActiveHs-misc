-- | Strict evaluation of diagrams with time and size limit
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Diagrams.Escape 
    ( escapeDiagram
    , numberErrors
    ) where

import Graphics.Diagrams
import Graphics.Diagrams.Types
import Graphics.Diagrams.Colors (toRGB)

import Data.Data.GenRep.Functions (getErrorIndex)
import Control.Exception.Pure (catchPureErrorsSafe)
import System.SimpleTimeout.Limits
import System.IO.Parallel (twoParallel)

import Control.Monad (liftM2)
import Control.DeepSeq (NFData, deepseq)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Control.Monad.State (State, runState)

-------------------------

-- | Strict evaluation of diagrams with time and size limit
escapeDiagram :: TimeLimit -> SizeLimit -> Diagram -> IO Diagram
escapeDiagram t s x = do
    ch <- newBudget t s
    idv <- newMVar 1        -- to generate identifiers for groups
    escapeDiagram' ch idv x

escapeDiagram' :: Budget -> MVar Int -> Diagram -> IO Diagram
escapeDiagram' ch idv = fff where

    fff :: Diagram -> IO Diagram
    fff x = do
        m <- checkBudget ch 1 
                (return . return . err . timeError) 
                (return . return $ err "size limit exceeded")
                $ fmap (either err id) (catchPureErrorsSafe x) >>= check
        m
      where
        timeError d
            = "timeout at " ++ show (round $ 100 * d :: Int) ++ "%" 

    err s = Error s EmptyDiagram

    m1 :: NFData a => (a -> Diagram) -> a -> IO (IO Diagram)
    m1 f a = fmap (return . either err f) $ catchPureErrorsSafe $ a `deepseq` a

    m2 :: NFData a => (a -> Diagram -> Diagram) -> a -> Diagram -> IO (IO Diagram)
    m2 f a b 
        = fmap (either (\s -> fmap (Error s) $ fff b) 
                       (\a -> fmap (f a) $ fff b)) 
            $ catchPureErrorsSafe $ a `deepseq` a

    checkColor :: Color -> Color
    checkColor (Color s) = case toRGB s of
        Just _  -> Color s
        _   -> error $ "Not an SVG color: " ++ s
    checkColor (RGB r g b) = RGB (f r) (f g) (f b)  where
        f x = 0 `max` (1 `min` x)

    check :: Diagram -> IO (IO Diagram)

    check EmptyDiagram      = return $ return EmptyDiagram
    check (Circle r)        = m1 Circle (if r >= 0 then r else error "negative radius") 
    check (Text p s)        = m1 (uncurry Text) (p, s)
    check (Rect a b)        = m1 (uncurry Rect) (a, b)
    check (Polyline loop l) = fmap g $ m1 (uncurry Polyline) (loop, l) where
        g m = m >>= \x -> case x of
            Polyline loop l -> do
                (b, l) <- decSizeBudget ch $ \x -> case splitAt (x+2) l of
                        (a,[]) -> (min x (x+2-length a), (True, a))
                        (a,_)  -> (0, (False, a))
                return $ if b 
                    then Polyline loop l
                    else Error "size limit exceeded" $ Polyline False l
            _ -> return x

    check (Link s x)        = m2 Link s x
    check (FontFamily s x)  = m2 FontFamily s x
    check (Move p x)        = m2 Move p x
    check (Scale t x)       = m2 Scale t x
    check (ScaleXY x y d)   = m2 (uncurry ScaleXY) (x,y) d
    check (Rotate t x)      = m2 Rotate t x
    check (Fill t x)        = m2 Fill (checkColor t) x
    check (Stroke t x)      = m2 Stroke (checkColor t) x
    check (StrokeWidth t x) = m2 StrokeWidth (if t >= 0 then t else error "negative stroke width") x
    check (Clip a b x)      = m2 (uncurry Clip) (a, b) x
    check (Error s x)       = m2 Error s x
    check (TransformMatrix a b c d e f x)
        = m2 (\(a,b,c,d,e,f) -> TransformMatrix a b c d e f) (a,b,c,d,e,f) x

    check (Pack x f) = return $ do
        i <- modifyMVar idv (\i -> return (i+1,i))
        fmap (\(x,y)-> Group x i y) $ twoParallel (fff x) (fff (f (Ref i)))

    check (Overlay a b) = return $ fmap (uncurry Overlay) $ twoParallel (fff a) (fff b)

    check (Ref i)       = return $ return $ Ref i
    check (Group _ _ _) = error "check: Group not possible"

-- | Error message extraction and numbering
numberErrors 
    :: Diagram
    -> (Diagram, [(String, String)])
numberErrors d = (res, reverse $ map swap errs)
 where
    swap (a,b) = (b,a)

    (res, (_, errs)) = runState (replace d) (0, [])

    m2 f x = fmap f $ replace x

    replace :: Diagram -> State (Int, [(String, String)]) Diagram
    replace (Link s x)        = m2 (Link s) x
    replace (FontFamily s x)  = m2 (FontFamily s) x
    replace (Move p x)        = m2 (Move p) x
    replace (Scale t x)       = m2 (Scale t) x
    replace (Rotate t x)      = m2 (Rotate t) x
    replace (Fill t x)        = m2 (Fill t) x
    replace (Stroke t x)      = m2 (Stroke t) x
    replace (StrokeWidth t x) = m2 (StrokeWidth t) x
    replace (Clip a b x)      = m2 (Clip a b) x
    replace (TransformMatrix a b c d e f x)
        = m2 (TransformMatrix a b c d e f) x
    replace (Group a i b)   = liftM2 (\a b -> Group a i b) (replace a) (replace b)
    replace (Overlay a b) = liftM2 Overlay (replace a) (replace b)
    replace (Error e x) = do
        i <- getErrorIndex e
        x <- replace x
        return $ Overlay x (i `textAt` (0,0) `fill` red)
    replace x = return x


