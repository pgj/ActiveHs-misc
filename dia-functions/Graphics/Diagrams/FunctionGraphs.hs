-- | Display function graphs and arcs
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Diagrams.FunctionGraphs where

import Graphics.Diagrams
--import Data.List (groupBy)

-------------------------

-- | The coordinate system.
coords :: Point -> Point -> Diagram
coords (a1,a2) (b1,b2) = union 
    [ (a1,0) ~~ (b1,0)
    , (b1-1,0.5) ~~ (b1,0)
    , (b1-1,-0.5) ~~ (b1,0)
    , (0.5,b2-1) ~~ (0,b2)
    , (-0.5,b2-1) ~~ (0,b2)
    , (0,a2) ~~ (0,b2)
    , (1,-0.5) ~~ (1,0.5)
    ]


-- | Draw the given function graph with gray coordinate system.
withCoords :: Point -> Point -> Diagram -> Diagram
withCoords a b d 
    = clip a b
        $   coords a b `stroke` gray
        <|> d `stroke` black `strokeWidth` 0.15


-- | Display a function defined on integer values.
displayDiscreteFun 
    :: (Integral a, Real b) 
    => Point                -- ^ display area left-bottom corner
    -> Point                -- ^ display area right-up corner
    -> (a -> b) 
    -> Diagram
displayDiscreteFun a@(a1,a2) b@(b1,b2) f 
    = withCoords a b $ union
        [ circle 0.1 `move` (fromIntegral x, y)
        | x <- [ceiling a1 .. floor b1]
        , let y = realToFrac (f x)
        , y <= b2 && y >= a2 ]


-- | Display a continuous function.
displayFun 
    :: (RealFrac a, Real b) 
    => Point                -- ^ display area left-bottom corner
    -> Point                -- ^ display area right-up corner
    -> (a -> b) 
    -> Diagram
displayFun a b f 
    = displayArc a b (fst a, fst b) $ \t -> (t, f t)


-- | Display an arc given by a function.
displayArc
    :: (Fractional a, Real b, Real c) 
    => Point                -- ^ display area left-bottom corner
    -> Point                -- ^ display area right-up corner
    -> (Double, Double)     -- ^ parameter interval
    -> (a -> (b, c))        -- ^ arc on the plain
    -> Diagram
displayArc a b (k1,k2) f 
    = withCoords a b $ joinPoints
          [ (realToFrac x, realToFrac y)
          | t <- [k1-0.1, k1.. k2+0.1]
          , let (x, y) = f (realToFrac t) ]



-- | Join points to form a continuous path with singularities.
joinPoints :: [Point] -> Diagram
joinPoints points =  union $ lines ++ dots
 where
    vectors = zipWith (\(a1,a2) (b1,b2) -> (b1-a1, b2-a2)) points (drop 1 points)

    cont = zipWith f vectors (drop 1 vectors)  where

        (a1,a2) `f` (b1,b2) 
            =  (a1*b1 + a2*b2) / sqrt ((a1*a1 + a2*a2)*(b1*b1 + b2*b2)) > 0.5

    join = zipWith (||) cont (drop 1 cont)

    lines = [ e ~~ f | (e, True, f) <- zip3 (drop 1 points) join (drop 2 points) ]

    dots = [ circle 0.1 `move` z 
           | (i, z, j) <- zip3 join (drop 2 points) (drop 1 join)
           , not (i && j)
           ]

{- because of an SVG error we can't do this more compact rendering:
joinPoints points =  union segments
 where
    vectors = zipWith (\(a1,a2) (b1,b2) -> (b1-a1, b2-a2)) points (drop 1 points)

    cont = zipWith f vectors (drop 1 vectors)  where

        (a1,a2) `f` (b1,b2) 
            =  (a1*b1 + a2*b2) / sqrt ((a1*a1 + a2*a2)*(b1*b1 + b2*b2)) > 0.5

    join = True : zipWith (||) cont (drop 1 cont)

    segments
        = map (mkSegment . map snd) $ groupBy (const fst) $ zip join $ drop 1 points

    mkSegment [z] = dot z
    mkSegment l = polyline l <|> dot (head l) <|> dot (last l)

    dot z = circle 0.1 `move` z 
-}
