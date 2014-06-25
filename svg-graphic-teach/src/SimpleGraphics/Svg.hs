module SimpleGraphics.Svg
    ( writeImage
    , writeImage'
    ) where

import SimpleGraphics.Types

type Properties = [(String,String)]

showColor :: Color -> String
showColor None    = "none"
showColor Black   = "black"
showColor Red     = "red"
showColor Green   = "green"
showColor Yellow  = "yellow"
showColor Blue    = "blue"
showColor Magenta = "magenta"
showColor Cyan    = "cyan"
showColor White   = "white"
showColor (RGB r g b)
    | r < 0 || r > 255 = error "red value out of range (0-255)"
    | g < 0 || g > 255 = error "green value out of range (0-255)"
    | b < 0 || b > 255 = error "blue value out of range (0-255)"
    | otherwise        = "rgb(" ++ show r
                              ++ ',' : show g
                              ++ ',' : show b ++ ")"


showTuple :: (Double,Double) -> String
showTuple (x,y) = show x ++ "," ++ show y

showTransform :: Transform -> [(String,String)]
showTransform (Fill c)      = [("fill",showColor c)]
showTransform (Opacity o)   = [("opacity",show o)]
showTransform (Stroke c w)  = [("stroke",showColor c),("stroke-width",show w)]
showTransform (Rotate a)    = [("transform","rotate(" ++ show a ++ ")")]
showTransform (Scale a b)   = [("transform","scale(" ++ show a ++ "," ++ show b ++ ")")]
showTransform (Translate v) = [("transform","translate(" ++ showTuple v ++ ")")]

simpleTag :: String -> Properties -> String
simpleTag n s = "<" ++ n
    ++ concatMap (\ (a,b) -> " " ++ a ++ "=" ++ show b) s ++ " />"

openTag :: String -> Properties -> String
openTag n s = "<" ++ n
    ++ concatMap (\ (a,b) -> " " ++ a ++ "=" ++ show b) s ++ ">"

closeTag :: String -> String
closeTag n = "</" ++ n ++ ">"

renderLines :: Graphic -> [String]
renderLines Empty             = []
renderLines (Rectangle w h)   = [simpleTag "rect" [("width",show w),("height",show h)]]
renderLines (Circle    r)     = [simpleTag "circle" [("r",show r)]]
renderLines (Ellipse   rx ry) = [simpleTag "ellipse" [("rx",show rx),("ry",show ry)]]
renderLines (Line      v1 v2) = [simpleTag "line"
                                    [ ("x1",show $ fst v1)
                                    , ("y1",show $ snd v1)
                                    , ("x2",show $ fst v2)
                                    , ("y2",show $ snd v2)
                                    ]]
renderLines (Polyline  s)     = [simpleTag "polyline" [("points",unwords . map showTuple $ s)]]
renderLines (Polygon   s)     = [simpleTag "polygon" [("points",unwords . map showTuple $ s)]]
renderLines (Over      a b)   = renderLines b ++ renderLines a
renderLines (Transform t a)   = [openTag "g" $ showTransform t]
                                ++ (map (' ':) . renderLines) a
                                ++ [closeTag "g"]
                                -- where
                                -- trn :: Graphic -> ([Transform],Graphic)
                                -- trn (Transform t' x) = (t':ts,g) where (ts,g) = trn x
                                -- trn x = ([],x)
                                -- (s,b) = trn a


render :: Graphic -> String
render = unlines . map (' ':) . renderLines

svgHeader :: String
svgHeader = "<?xml version=\"1.0\" standalone=\"no\"?>\n"
    ++ "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\""
    ++ " \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n"

-- | Ulozi graficky objekt do SVG souboru.
--
-- > writeImage "kolecko.svg" (translate (50,50) (circle 20))
writeImage :: FilePath -> Graphic -> IO ()
writeImage n a = writeFile n $
    svgHeader
    ++ "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">\n"
    ++ render a
    ++ "</svg>"

-- | Ulozi graficky objekt do SVG souboru, poznaci oblast zobrazenia (view-box).
--
-- > writeImage' 100 100 "kolecko.svg" (translate (50,50) (circle 20))
writeImage' :: Double -> Double -> FilePath -> Graphic -> IO ()
writeImage' x y n a = writeFile n $
    svgHeader
    ++ "<svg viewBox=\"0 0 " ++ show x ++ " " ++ show y ++ "\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">\n"
    ++ render a
    ++ "</svg>"
