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

initialPointsSimple :: SimplePolygon -> [Point]
initialPointsSimple s@(Simple pts) = zipWith (+) deltaVecs pts
    where deltaVecs = map (scale (1 / 2 ** 9)) $ inwardFacingVectors s

initialPoints :: Polygon -> [Point]
initialPoints (Polygon outer holes) = concatMap initialPointsSimple (outer : holes)
