module SimpleGraphics.Types
    ( Angle
    , Point
    , Vector
    , Color (..)
    , PathElement (..)
    , Graphic (..)
    , Transform (..)
    ) where

-- | Typove synonymum pro uhel.
type Angle  = Double

-- | Typove synonymum pro bod.
type Point  = (Double,Double)

-- | Typove synonymum pro vektor.
type Vector = (Double,Double)

-- | Pojmenovane barvy s moznosti 'zadna barva' a moznosti udat vlastni RGB kombinaci.
-- (Poskytnute udaje o barve jsou prebrany z
-- <http://www.w3.org/TR/SVG/types.html#ColorKeywords>.)
data Color
    = None
    -- ^ Zadna barva. Pri vybarveni objektu touto barvou bude objekt pruhledny.
    | Black
    -- ^ Cerna. @rgb(0,0,0)@
    | Red
    -- ^ Cervena. @rgb(255,0,0)@
    | Green
    -- ^ Zelena. @rgb(0,128,0)@
    | Yellow
    -- ^ Zluta. @rgb(255,255,0)@
    | Blue
    -- ^ Modra. @rgb(0,0,255)@
    | Magenta
    -- ^ Purpurova/fialova. @rgb(255,0,255)@
    | Cyan
    -- ^ Azurova/modrozelena. @rgb(0,255,255)@
    | White
    -- ^ Bila. @rgb(255,255,255)@
    | RGB Int Int Int
    -- ^ Vlastni barevna kombinace. Postupne hodnota pro cervenou (Red) zelenou (Green) a modrou (Blue).
    -- Pripustne jsou hodnoty v rozpeti 0-255.

-- | Komponenty na popis cesty.
data PathElement
    = MoveToAbs Point
    | MoveToRel Vector
    | ClosePath
    | LineToAbs Point
    | LineToRel Point
    | CurveToAbs Point Point Point
    -- ^ Bezierova krivka, absolutne poziciovanie.
    | CurveToRel Point Point Point
    -- ^ Bezierova krivka, relativne poziciovanie.
    | SCurveToAbs Point Point
    -- ^ Plynula (smooth) Bezierova krivka, absolutne poziciovanie. Prvy bod je obrazom predpredchodzieho v bodovej sumernosti so stredom v predchodzom bode.
    | SCurveToRel Point Point
    -- ^ Ako SCurveToAbs, ale relativne poziciovanie.

-- | Graficke objekty.
data Graphic
    = Empty
    | Rectangle  Double Double
    | Circle     Double
    | Ellipse    Double Double
    | Line Point Point
    | Polyline   [Point]
    | Polygon    [Point]
    | Path       [PathElement]
    | Over       Graphic Graphic
    | Transform  Transform Graphic

data Transform
    = Fill      Color
    | Opacity   Double
    | Stroke    Color Double
    | Rotate    Angle
    | Scale     Double Double
    | Translate Vector

