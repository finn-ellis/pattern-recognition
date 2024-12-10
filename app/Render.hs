module Render where

import Types ( State (..), Pattern )
import Consts ( strokeRadius )
import Recognition (normalizePattern, scalePattern)
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as Point
import qualified Graphics.Gloss.Data.Vector as Vector
import Data.Maybe (fromMaybe)



{-   PATH RENDERING   -}
thickSegment :: Float -> Point -> Point -> Picture
thickSegment radius p1 p2 =
    let
        diff = p2 Point.- p1
        dir  = radius Point.* Vector.normalizeV diff
        pdir = (-(snd dir), fst dir)
    in Polygon [ p1 Point.- pdir
               , p1 Point.- dir
               , p1 Point.+ pdir
               , p2 Point.+ pdir
               , p2 Point.+ dir
               , p2 Point.- pdir
               ]

thickLine :: Float -> Path -> Picture
thickLine radius ps = Pictures [thickSegment radius p np | (p, np) <- zip ps (tail ps)]

linePoints :: Path -> Picture
linePoints = Pictures . map drawPoint
    where
        drawPoint (x, y) = translate x y
                         $ Color red
                         $ circleSolid (strokeRadius*0.5)

renderPattern :: Float -> Pattern -> Bool -> Picture
renderPattern strRad ls showPs = Pictures (map (thickLine strRad) ls ++ if showPs then map linePoints ls else [])

renderPatDef :: Pattern -> Bool -> Picture
renderPatDef = renderPattern strokeRadius

{-   BUTTON RENDERING   -}
-- renderButton btn = undefined

-- btns = []

-- renderButtons :: Picture
-- renderButtons = Pictures (map renderButton btns)

{-   RECOGNITION RENDERING   -}
toCorner :: (Int, Int) -> Picture -> Picture
toCorner (sx, sy) = translate (fromIntegral (-sx) / 2) (fromIntegral (-sy) / 2)

renderRecognition :: State -> Picture
renderRecognition s = translate 20 20 $ Color blue $ Pictures
        [ Text (recognized s ++ "(" ++ show (recN s) ++ ")")
        , translate 0 100 $ renderPattern 2 (scalePattern 200 recPat) False
        , translate 0 100 $ Color black $ renderPattern 4 (scalePattern 200 $ normalizePattern (pattern s)) False
        ]
    where
        recPat :: Pattern
        recPat = fromMaybe [] (lookup (recognized s) (patterns s))

render :: (Int, Int) -> State -> Picture
render ss s@S { pattern    = ls
              , showPoints = vis
              } = Pictures [ renderPatDef ls vis
                           , toCorner ss $ renderRecognition s
                           ]