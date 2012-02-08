{-# LANGUAGE DeriveDataTypeable #-}
-- | Diagrams data types
module Graphics.Diagrams.Types where

import Data.Data (Data, Typeable, dataTypeOf, mkDataType, gunfold, toConstr)
import Control.DeepSeq (NFData, rnf)

-----------------------

-- | Point type as defined in the diagrams package
type Point = (Double, Double)

-- | Diagram data type
data Diagram 
    = EmptyDiagram          -- ^ empty diagram
    | Circle Double         -- ^ circle with radius
    | Rect Double Double    -- ^ rectangle; width and height
    | Polyline Bool [Point] -- ^ True: polygon; False: polyline
    | Text Position String  -- ^ text at a given position

    | Move Point Diagram        -- ^ move
    | Scale Double Diagram      -- ^ scale
    | ScaleXY Double Double Diagram -- ^ scale differently at x and y axes
    | Rotate Double Diagram         -- ^ rotate (degree)
    | TransformMatrix Double Double Double Double Double Double Diagram  -- ^ used internally
    | Clip Point Point Diagram      -- ^ clip a rectangle region (lower-left and upper-right corners)

    | Fill Color Diagram            -- ^ fill with color
    | Stroke Color Diagram          -- ^ set stroke color
--    | FillOpacity Double Diagram    -- ^ set fill opacity
--    | StrokeOpacity Double Diagram  -- ^ set stroke opacity
    | StrokeWidth Double Diagram    -- ^ set stroke width
    | FontFamily String Diagram     -- ^ set font family
    | Link String Diagram           -- ^ add an html link

    | Overlay Diagram Diagram       -- ^ overlay (the second diagram is over the first one)
    | Pack Diagram (Diagram -> Diagram)   -- ^ pack a diagram (kind of let-construct to save resources)

    | Group Diagram Int Diagram  -- ^ used internally
    | Ref Int                 -- ^ used internally
    | Error String Diagram    -- ^ used internally
        deriving (Typeable)

instance Data Diagram where
    dataTypeOf _ = mkDataType "Diagram" []
    gunfold _ _ = error "gunfold is not defined on the Diagram datatype"
    toConstr _ = error "toConstr is not defined on the Diagram datatype"

-- | Text positions
data Position
    = Start    -- ^ beginning of text is fixed
    | Middle   -- ^ middle of text is fixed
    | End      -- ^ end of text is fixed
        deriving (Eq, Ord, Show, Data, Typeable)

instance NFData Position where
    rnf x = x `seq` ()

-- | colors
data Color
    = Color String          -- ^ named color
    | RGB Double Double Double  -- ^ RGB color (components are between 0 and 1)
      deriving (Eq, Ord, Show, Data, Typeable)

instance NFData Color where
    rnf (Color s) = rnf s
    rnf (RGB r g b) = rnf (r, g, b)


