module Input where

import Types ( State (..), Pattern )
import Consts ( segmentLength )
import Recognition (normalizePattern, scalePattern)
import Graphics.Gloss.Interface.IO.Game
import qualified Graphics.Gloss.Data.Point.Arithmetic as Point
import qualified Graphics.Gloss.Data.Vector as Vector


{-   INPUT   -}
beginPath :: (Float, Float) -> State -> State
beginPath p s@S { pattern = ls} = s { drawing    = True
                                    , showPoints = True
                                    , pattern    = [p]:ls
                                    }

addToPath :: (Float, Float) -> State -> State
addToPath _ s@S { drawing = False } = s
addToPath p s@S { drawing = True }  =
    case pattern s of
        []              -> beginPath p s
        []:ps           -> s { pattern = [p]:ps }
        [lp]:ps         -> s { pattern = (p:[lp]):ps }
        (lp:llp:rps):ps ->
            let
                dist = Vector.magV (llp Point.- p)
            in if dist > segmentLength
                then s { pattern = (p:lp:llp:rps):ps }
                else s { pattern = (p:llp:rps):ps }

endPath :: (Float, Float) -> State -> State
endPath _ s =
    let
        ns = s { drawing = False, showPoints = False }
    in case pattern ns of
        []         -> ns
        []:ps      -> ns { pattern = ps }
        [_]:ps     -> ns { pattern = ps }
        (lp:rp):ps ->
            let
                fp = last rp
                np = if Vector.magV (lp Point.- fp) < segmentLength
                     then fp:lp:rp
                     else lp:rp
            in ns { pattern = np:ps}

input :: Event -> State -> State
input (EventKey (MouseButton LeftButton) Down _ p) = beginPath p
input (EventKey (MouseButton LeftButton) Up _ p)   = endPath p
input (EventKey (SpecialKey KeySpace) Down _ _)    = \s -> s { showPoints = True }
input (EventKey (SpecialKey KeySpace) Up _ _)      = \s -> s { showPoints = False }
input (EventKey (SpecialKey KeyDelete) Down _ _)   = \s -> s { pattern = [] }
input (EventKey (SpecialKey KeyEnter) Down _ _)    = \s@S{ pattern = ps} -> s { pattern = scalePattern 500 $ normalizePattern ps }
input (EventMotion p)                              = addToPath p
input _                                            = id

getInput :: (Pattern -> IO ()) -> Event -> State -> IO State
getInput writePattern (EventKey (Char 'S') Down _ _) s@S{ pattern = p} = do
    _ <- writePattern p
    return s { pattern = scalePattern 500 $ normalizePattern p }
getInput _ i s = return (input i s)

{-
idea for recongition:
    1) normalize drawing coordinates based on extents (maintain aspect ratio)... this should be an easy fun function
    2) calculate similarity rating: sum of dists from each point to its closest point on the template (normalized)
    3) lowest similarity rating wins
    4) template / program could have maximum similarity so that drawings that dont match anything arent selected

foreseen issues:
    1) what if a drawing coincidentally has close points, but...
        - misses critical points
            ===> (each template point has to 'claim' its closest point... then check if any are left without claimed points)
        - has close and far points such that it is 'more similar' than a drawing which has most of its points mediumly off from the desired template
            ===> square of distance from closest point?
-}