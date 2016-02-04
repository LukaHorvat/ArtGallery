module Domain.Initial where

import Geometry
import Common

inwardFacingVectors :: SimplePolygon -> [Vector]
inwardFacingVectors (Simple pts) = map inward $ slidingWindow 3 loop
    where loop = last pts : pts ++ [head pts]
          inward [a, b, c] = normalize $ normalize vec1 + normalize vec2
              where vec1 = rotate (a `vecTo` b) rotAngle
                    vec2 = rotate (b `vecTo` c) rotAngle
          inward _         = error "slidingWindow returned a list with more than 3 elements"
          rotAngle = pi / 2

initialPointsSimple :: IsHole -> SimplePolygon -> [Point]
initialPointsSimple hole poly = zipWith (+) deltaVecs pts
    where deltaVecs = map (scale (1 / 2 ** 9)) $ inwardFacingVectors s
          s@(Simple pts) = rewind hole poly

rewind :: IsHole -> SimplePolygon -> SimplePolygon
rewind hole poly
    | not hole && initialWind == CW || hole && initialWind == CCW = reverseSimple poly
    | otherwise = poly
    where initialWind = winding poly

initialPoints :: Polygon -> [Point]
initialPoints (Polygon outer holes) =
    initialPointsSimple False outer ++ concatMap (initialPointsSimple True) holes
