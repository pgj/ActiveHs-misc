-- | Diagrams SVG backend
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Diagrams.SVG 
    ( render
    ) where

import Graphics.Diagrams
import Graphics.Diagrams.Types
import Graphics.Diagrams.Escape (escapeDiagram, numberErrors)

import System.SimpleTimeout.Limits

import Text.XHtml.Strict (Html, tag, strAttr, (+++), (<<), (!), intAttr, toHtml, noHtml)

import Data.Char (intToDigit, showLitChar, ord)
import Data.List (intercalate, groupBy, sortBy)
import Data.Function (on)
import Numeric (showFFloat)

-------------------------

data SVG 
    = G String [(String, String)] [SVG] (Maybe SVG)
    | GText String

mkSVG :: String -> [String] -> [String] -> SVG
mkSVG n t l = G n (zip t l) [] Nothing

addAtt :: (String, String) -> SVG -> SVG
addAtt a@(x,_) (G n as l Nothing) 
    | [j | (i,j)<-as, i == x] /= [""] 
    = G n (a:as) l Nothing
addAtt a g = G "g" [a] [g] Nothing

mkFun :: String -> [String] -> String
mkFun f l = f ++ "(" ++ intercalate "," l ++ ")"

mkStyle :: String -> String -> SVG -> SVG
mkStyle f c = addAtt ("style", f ++ ": " ++ c)

mkTransform :: SVG -> String -> SVG
g `mkTransform` s = addAtt ("transform", s) g


showF :: Double -> String
showF x = showFFloat (Just 3) x ""

showColor :: Color -> String
showColor (Color s) = s
showColor (RGB r g b) = "#" ++ f r ++ f g ++ f b where
    f x = [intToDigit (i `div` 16 `mod` 16), intToDigit (i `mod` 16)] where
        i = round (x * 255)

---------------

-- | An idetifier is needed
-- because browsers maintain global xlink-s
toSVGWithId :: String -> Diagram -> SVG
toSVGWithId idi = toSVG where

    toSVG :: Diagram -> SVG
    toSVG EmptyDiagram  = G "g" [] [] Nothing
    toSVG (Circle r)    = mkSVG "circle" ["r"] [showF r]
    toSVG (Text pos s)  = G "text" [("style","text-anchor: " ++ showPos pos)] [GText $ concatMap escape s] Nothing
      where
        escape '\\' = "\\"
        escape c | ord c >= 161 = [c]
        escape c = showLitChar c ""

        showPos Start = "start"
        showPos Middle = "middle"
        showPos End = "end"

    toSVG (Link s x)    = G "a" [("xlink:href", s)] [toSVG x] Nothing
    toSVG (Rect a b)    = mkSVG "rect" ["x","y","width","height"] $ map showF [-a/2, -b/2, a, b]
    toSVG (Polyline _ [(x1, y1), (x2, y2)]) 
                        = mkSVG "line" ["x1","y1","x2","y2"] $ map showF [x1, y1, x2, y2]
    toSVG (Polyline loop l) 
            = mkSVG (if loop then "polygon" else "polyline") ["points"] 
                [unwords [showF x ++ "," ++ showF y | (x,y)<-l]]

    toSVG a@(Overlay _ _) = G "g" [] (concatMap (unG . toSVG) $ collectU [] a) Nothing where

        unG (G "g" [] l Nothing) = l
        unG x = [x]

        collectU acc EmptyDiagram = acc
        collectU acc (Overlay a b) = collectU (collectU acc b) a
        collectU acc x = x: acc

    toSVG (Group d i s) = addDef [addAtt ("id", idi ++ show i) $ toSVG d] $ toSVG s  where
        addDef [] x = x
        addDef l (G "defs" [] l' (Just x)) = addDef (l++l') x
        addDef l x = G "defs" [] l $ Just x

    toSVG (Ref i) = mkSVG "use" ["xlink:href"] ["#" ++ idi ++ show i]
    toSVG (Move (a,b) x)    = toSVG x `mkTransform` mkFun "translate" [showF a, showF b]
    toSVG (Scale t x)       = toSVG x `mkTransform` mkFun "scale" [showF t, showF t]
    toSVG (ScaleXY tx ty x) = toSVG x `mkTransform` mkFun "scale" [showF tx, showF ty]
    toSVG (Rotate d x)      = toSVG x `mkTransform` mkFun "rotate" [showF d]
    toSVG (FontFamily s x)  = mkStyle "font-family" s           $ toSVG x
    toSVG (Fill c x)        = mkStyle "fill"   (showColor c)    $ toSVG x
    toSVG (Stroke c x)      = mkStyle "stroke" (showColor c)    $ toSVG x
    toSVG (StrokeWidth w x) = mkStyle "stroke-width" (showF w)  $ toSVG x
    toSVG (TransformMatrix a b c d e f x) 
        = toSVG x `mkTransform` ("matrix(" ++ intercalate " " (map showF [a,b,c,d,e,f]) ++ ")")
    toSVG (Clip (a1,a2) (b1,b2) x)  = toSVG x -- inner clip is not implemented yet
    toSVG (Error s x)               = toSVG x  -- not possible
    toSVG (Pack x f)    = error "toSVG: use escape first"



---------------------------------------------------

showSVG :: SVG -> Html
showSVG x = case x of
    GText s    -> toHtml s
    G n as l e -> (tag n !
            [ strAttr a $ intercalate (sep a) is
            | (a:_,is)<- map unzip $ groupBy ((==) `on` fst) $ sortBy (compare `on` fst) as]
                    << map showSVG l)
        +++ case e of
                Nothing -> noHtml
                Just x -> showSVG x
  where
    sep "transform" = " "
    sep _ = "; "

addSVGHeader :: Int -> Int -> Html -> Html
addSVGHeader w h =
    tag "svg" ! [ strAttr "xmlns" "http://www.w3.org/2000/svg"
                , strAttr "xmlns:xlink" "http://www.w3.org/1999/xlink" 
                , strAttr "version" "1.1"
                , strAttr "baseProfile" "full" 
                , intAttr "width" w
                , intAttr "height" h
                ] 


render 
    :: Double       -- ^ magnification
    -> Point        -- ^ bottom-left corner
    -> Point        -- ^ top-right corner
    -> TimeLimit    -- ^ time limit
    -> SizeLimit    -- ^ size limit
    -> String       -- ^ identifier (needed only if there are several SVG picture on a page)
    -> Diagram      -- ^ diagram to show
    -> IO (Html, [(String,String)]) -- ^ xml code and error messages
render mag bottomLeft topRight t s idi x = do
    xx <- escapeDiagram t s x
    let (x', err) = numberErrors xx
    return (f x', err)
 where
    f (Clip a b x) = g a b x
    f x = g bottomLeft topRight x

    g (a1,a2) (b1,b2) s =
        addSVGHeader (round $ mag * (b1 - a1)) (round $ mag * (b2 - a2)) 
            << showSVG (
            toSVGWithId idi
                $ TransformMatrix mag 0 0 (-mag) (-mag*a1) (mag*b2) s `fill` white `stroke` black `strokeWidth` 0.1)






