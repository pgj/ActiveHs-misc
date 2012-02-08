-- | Diagrams user API
module Graphics.Diagrams 
    ( -- * Points
      Point
    , (*.), (.*.), (.+.), (.-.)
      -- * Colors
    , Color
    , color, rgb
    , red, green, blue, black, white, gray, yellow
      -- * Diagrams
    , Diagram
      -- ** Primitives
    , empty, rect, rectangle, circle, (>-<), (~~), polyline, polygon
      -- ** Texts
    , textFrom, textTo, textAt
      -- ** Transformations
    , move, rotate, scale, scaleXY, clip
      -- ** Styles
    , fill, stroke, strokeWidth, fontFamily, link
      -- ** Combining diagrams
    , union, (<|>)
    , pack
    ) where

import Graphics.Diagrams.Types

---------------------

infixl 7 .*., *.
infixl 6 .+., .-.
infix  4 >-<, ~~
infixl 3 `move`, `rotate`, `scale`, `scaleXY`, `fill`, `stroke`, `strokeWidth`, `textFrom`, `textTo`, `textAt`, `fontFamily`, `link`
infixl 2 <|>

--------------------

-- | empty diagram
empty :: Diagram
empty = EmptyDiagram

-- | line segment
(>-<), (~~), line :: Point -> Point -> Diagram
(>-<) = line
(~~) = line
line a b = polyline [a,b] 

-- | rectangle given with width and height
rect :: Double -> Double -> Diagram
rect = Rect


-- | rectangle given with two opposite corners
rectangle :: Point -> Point -> Diagram
rectangle (a1,a2) (b1,b2) = rect (abs (b1-a1)) (abs (b2-a2)) `move` (a1+(b1-a1)/2, a2+(b2-a2)/2)

--dot :: Diagram
--dot = circle 0.5 `strokeWidth` 0 `fill` black

-- | circle with radius
circle :: Double -> Diagram
circle = Circle

polygon, polyline :: [Point] -> Diagram
polygon = Polyline True
polyline = Polyline False

-- | text in origo
text :: Position -> String -> Diagram
text x  = TransformMatrix 0.1 0 0 (-0.1) 0 0 . Text x

-- | text; middle of text is fixed
textAt :: String -> Point -> Diagram
textAt t x = text Middle t `move` x

-- | text; beginning of text is fixed
textFrom :: String -> Point -> Diagram
textFrom t x = text Start t `move` x

-- | text; end of text is fixed
textTo :: String -> Point -> Diagram
textTo t x = text End t `move` x

-- | set font family
fontFamily :: Diagram -> String -> Diagram
fontFamily = flip FontFamily

-- | add an html link
link :: Diagram -> String -> Diagram
link = flip Link

-- | clip a rectangle region (give lower-left and upper-right corners)
clip :: Point -> Point -> Diagram -> Diagram
clip = Clip


move :: Diagram -> Point -> Diagram
x `move` p = Move p x

-- | rotate (degree)
rotate :: Diagram -> Double -> Diagram
x `rotate` d = Rotate d x

-- | scale differently at x and y axes
scale :: Diagram -> Double -> Diagram
x `scale` t = Scale t x

scaleXY :: Diagram -> (Double, Double) -> Diagram
d `scaleXY` (x, y) = ScaleXY x y d

-- | fill with color
fill :: Diagram -> Color -> Diagram
x `fill` c = Fill c x

-- | set stroke color
stroke :: Diagram -> Color -> Diagram
x `stroke` c = Stroke c x 

-- | set stroke width
strokeWidth :: Diagram -> Double -> Diagram
x `strokeWidth` c = StrokeWidth c x

-- | overlay; the second diagram is over the first one
(<|>) :: Diagram -> Diagram -> Diagram
(<|>) = Overlay

-- | overlay generalized to several diagrams
union :: [Diagram] -> Diagram
union = foldr Overlay EmptyDiagram

-- | pack a diagram; (@pack d f@) is the more efficient version of (@let v=d in f v@).
pack :: Diagram -> (Diagram -> Diagram) -> Diagram
pack = Pack



-------------------------

-- | named color
color :: String -> Color
color = Color

-- | RGB color (components are between 0 and 1)
rgb :: Double -> Double -> Double -> Color
rgb = RGB

red, green, blue, black, white, gray, yellow :: Color
red    = color "red"
green  = color "green"
blue   = color "blue"
black  = color "black"
white  = color "white"
gray   = color "gray"
yellow = color "yellow"


-- | Scalar multiplication.
(*.) :: Double -> Point -> Point
s *. (x,y) = (s*x, s*y)

-- | Elementwise addition, subtraction and multiplication for 'Point's.
(.+.), (.-.), (.*.) :: Point -> Point -> Point
(x1,y1) .+. (x2,y2) = (x1 + x2, y1 + y2)
(x1,y1) .*. (x2,y2) = (x1 * x2, y1 * y2)
a .-. b = a .+. ((-1) *. b)




