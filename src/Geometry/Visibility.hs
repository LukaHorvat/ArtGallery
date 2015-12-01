module Geometry.Visibility where

import Geometry.Point
import Geometry.Polygon
import Data.List (sortBy, minimumBy)
import Data.Ord
import Data.Maybe (mapMaybe)

visibilityPolygon :: Point -> Polygon -> SimplePolygon
visibilityPolygon start poly = Simple cont
    where verts        = vertices poly
          cont         = sortBy (comparing vertDir) (concatMap cast verts)
          cast vert    = map (\v -> rayCast start v poly) (jitter $ start `vecTo` vert)
          vertDir vert = direction $ start `vecTo` vert

intersect :: Point -> Vector -> Segment -> Maybe Point
intersect p r (Segment q q')
    | t <= 0           = Nothing
    | u >= 0 && u <= 1 = Just (q + u `scale` s)
    | otherwise        = Nothing
    where s = q `vecTo` q'
          t = ((q - p) `cross` s) / (r `cross` s)
          u = ((p - q) `cross` r) / (s `cross` r)

rayCast :: Point -> Vector -> Polygon -> Point
rayCast start dir (Polygon outer holes) = minimumBy (comparing (sqrDist start)) inters
    where segs   = segments outer ++ concatMap segments holes
          inters = mapMaybe (start `intersect` dir) segs

vertices :: Polygon -> [Point]
vertices (Polygon (Simple outer) holes) = outer ++ concatMap (\(Simple inner) -> inner) holes
