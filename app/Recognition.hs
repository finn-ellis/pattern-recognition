module Recognition where

import Types (Pattern)
import Consts (maxDistance)
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as Point
import qualified Graphics.Gloss.Data.Vector as Vector

defaultBounds :: (Point, Point)
defaultBounds = ((1e6, 1e6), (1e-6, 1e-6))


{-   UTILS   -}
bounds :: Point -> (Point, Point) -> (Point, Point)
bounds (x, y) ((minx, miny), (maxx, maxy)) = ( (min x minx, min y miny)
                                             , (max x maxx, max y maxy))

-- Path -> (min, max)
getBounds :: Path -> (Point, Point)
getBounds = foldr bounds defaultBounds

mergeBounds :: (Point, Point) -> (Point, Point) -> (Point, Point)
mergeBounds (aMin, aMax) = bounds aMax . bounds aMin

normalizePath :: (Point, Point) -> Path -> Path
normalizePath (minb, maxb) = map normalize
    where
        range       = maxb Point.- minb
        size        = uncurry max range
        small       = uncurry min range
        offset      = ((size - small)/2) Point.* (if uncurry (>) range then (0, 1) else (1, 0))
        normalize p = let (dx, dy) = p Point.- minb Point.+ offset 
                      in (dx/size, dy/size)

normalizePattern :: Pattern -> Pattern
normalizePattern ps =
    let totBnds = foldr (mergeBounds . getBounds) defaultBounds ps
    in map (normalizePath totBnds) ps

scalePath :: Float -> Path -> Path
scalePath n = map (n Point.*)

scalePattern :: Float -> Pattern -> Pattern
scalePattern n = map (scalePath n)

-- find distance to closest point in Pattern
-- more efficient way to do this?
distToClosest :: Pattern -> Point -> Float
distToClosest pat p = foldr min 1e6 pDists
    where
        -- pDists = [ Vector.magV (p Point.- ap) | apath <- pat, ap <- apath ]
        -- hack: square the distance so points that are further away are more significant
        pDists = [ (\x -> Vector.dotV x x) (p Point.- ap) | apath <- pat, ap <- apath ]


patDist :: Pattern -> Pattern -> Float
-- patDist tmpl pat = sum $ map (distToClosest tmpl) ps
--     where
--         ps = [ap | apath <- pat, ap <- apath]
-- change: use points on tmpl for distance so that there are no "missed" points
-- issue: extra points in the pat are ignored...
-- is checking the pattern both ways and adding the distances the only way to solve this? this doubles complexity
patDist patA patB = sum $ map (distToClosest patB) ps
    where
        ps = [ap | apath <- patA, ap <- apath]

lowestDist :: [(String, Pattern)] -> Pattern -> (String, Float)
lowestDist tmpls pat = foldr go ("", 1e6) tmpls
    where
        -- go both ways for most accurate distance calculation
        go (tn, tmpl) (mn, mdist) = let dist = patDist tmpl pat + patDist pat tmpl
                                    in if dist < mdist
                                       then (tn, dist)
                                       else (mn, mdist)

recPatDist :: [(String, Pattern)] -> Pattern -> (String, Float)
recPatDist _ []   = ("", 0)
recPatDist tmpl p = let (name, dist) = lowestDist tmpl (normalizePattern p)
                    in if dist < maxDistance then (name, dist) else ("", 0)