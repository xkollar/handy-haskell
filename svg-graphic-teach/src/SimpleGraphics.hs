-- |
-- Module       : SimpleGraphics
-- Copyright    : Matej Kollar
-- License      : BSD3
--
-- Maintainer   : Matej Kollar <xkollar2@fi.muni.cz>
-- Stability    : provisional
-- Portability  : portable
--
-- Maly modul na jednoduchou praci s grafikou.
-- Moznost ukladani do SVG. Primarne vyvijeno pro
-- potreby vyuky v stredisku Teiresias <http://teiresias.muni.cz/>.

module SimpleGraphics
    ( -- * Typova synonyma
      Angle
    , Point
    , Vector

      -- * Datove typy
    , Color (..)
    , Graphic

      -- * Zakladni graficke objekty
    , emptyGraphic
    , rectangle
    , circle
    , ellipse
    , line
    , polyline
    , polygon
    , overGraphic
    , overGraphics

      -- * Transformace grafickych objektu
    , flipX
    , flipY
    , opacity
    , rotate
    , scale
    , scale'
    , translate
    , withFill
    , withStroke

    , writeImage
    ) where

import SimpleGraphics.Svg ( writeImage )
import SimpleGraphics.Types

-- Graphic primitives

-- | Funkce 'emptyGraphic' vytvori prazdny graficky objekt. Tento objekt nic
-- nekresli. Pouziti zejmena pri ruznem skladani grafickych objektu.
--
-- > foldr overGraphic emptyGraphic (map circle [10,20..100])
emptyGraphic :: Graphic
emptyGraphic = Empty

-- | Funkce 'rectangle' vytvori obdelnik s odpovidajici sirkou a vyskou.
-- Levy horni roh je v bode @(0,0)@.
--
-- > rectangle 35 20
rectangle :: Double -> Double -> Graphic
rectangle = Rectangle

-- | Funkce 'circle' vytvori kruh s danym polomerem.
-- Stred je v bode @(0,0)@.
--
-- > circle 60
circle :: Double -> Graphic
circle = Circle

-- | Funkce 'ellipse' vytvori elipsu danou polomery na ose x a ose y.
-- Stred je v bode @(0,0)@.
--
-- > ellipse 60 40
ellipse :: Double -> Double -> Graphic
ellipse = Ellipse

-- | Funkce 'line' vytvori caru mezi dvema body.
--
-- > line (100,90) (30,80)
line :: Point -> Point -> Graphic
line = Line

-- | Funkce 'polyline' vytvori lomenou caru popsanou seznamem bodu.
--
-- > polyline [(10,10),(20,10),(20,20),(30,20),(30,10),(40,10)]
polyline :: [Point] -> Graphic
polyline = Polyline

-- | Funkce 'polygon' vytvori mnohouhelnik popsany seznamem bodu.
--
-- > polygon [(350,75),(379,161),(469,161),(397,215),(423,301),(350,250),(277,301),(303,215),(231,161),(321,161)]
polygon :: [Point] -> Graphic
polygon = Polygon

-- Graphics composition

-- | Spoji dva graficke objekty tak, ze prvni umisti pred druhy.
--
-- > withFill Red (circle 20) `overGraphic` circle 40
overGraphic :: Graphic -> Graphic -> Graphic
overGraphic = Over

-- | Spoji vice grafickych objektu tak, ze prvni v seznamu je pomyslne nejvys.
overGraphics :: [Graphic] -> Graphic
overGraphics = foldr overGraphic Empty

-- Transformation

-- | Preklopi (zrkadlove) graficky objeky kolem osi x.
--
-- > flipX (polygon [(-1,1),(1,0),(1,2)])
flipX :: Graphic -> Graphic
flipX = scale' 1 (-1)

-- | Preklopi (zrkadlove) graficky objeky kolem osi y.
--
-- > flipX (polygon [(-1,1),(1,0),(1,2)])
flipY :: Graphic -> Graphic
flipY = scale' (-1) 1

-- | Nastavi pruhlednost objektu.
-- Pruhlednost je z intervalu 0 az 1. Hodnota 0 pro uplne pruhledny, 1 pro uplne nepruhledny.
-- Pruhlednost je kumulativni.
--
-- > opacity 0.5 (withFill Red (circle 20)) `overGraphic` translate (10,0) (withFill Green (circle 20))
opacity :: Double -> Graphic -> Graphic
opacity = Transform . Opacity

-- | Otoci graficky objekt o dany uhel v stupnich kolem bodu @(0,0)@.
--
-- > rotate 45 (rectangle 20 30)
rotate :: Angle -> Graphic -> Graphic
rotate = Transform . Rotate

-- | Proporcionalne zmeni rozmery nasobkem cisla.
--
-- > scale 2.5 (line (10,10) (10, 100))
scale :: Double -> Graphic -> Graphic
scale x = Transform $ Scale x x

-- | Proporcionalne zmeni rozmery nasobkem cisla samostatne pre os x a os y.
--
-- > scale' 2 3  (circle 10)
scale' :: Double -> Double -> Graphic -> Graphic
scale' x y = Transform $ Scale x y

-- | Posune graficky objekt o dany vektor.
--
-- > translate (300,200) (ellipse 30 50)
translate :: Vector -> Graphic -> Graphic
translate = Transform . Translate

-- | Vnitrek objektu vyplni danou barvou.
-- Funguje jen na objektech, kterym jeste barva vyplne nebyla prirazena.
--
-- > withFill Green (circle 10)
withFill :: Color -> Graphic -> Graphic
withFill = Transform . Fill

-- | Objekt obtahne linkou s danou barvou a hroubkou.
-- Funguje jen na objektech, kterym jeste barva linky nebyla prirazena.
--
-- > withStroke Blue 1 (rectangle 30 40)
withStroke :: Color -> Double -> Graphic -> Graphic
withStroke c = Transform . Stroke c

