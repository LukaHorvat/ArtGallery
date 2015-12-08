module Geometry.Polygon where

import Geometry.Point

data    Segment       = Segment Point Point                   deriving (Eq, Ord, Read, Show)
newtype SimplePolygon = Simple [Point]                        deriving (Eq, Ord, Read, Show)
data    Polygon       = Polygon SimplePolygon [SimplePolygon] deriving (Eq, Ord, Read, Show)

pairwise :: [a] -> [(a, a)]
pairwise [] = []
pairwise xs = zip xs (tail xs)

segments :: SimplePolygon -> [Segment]
segments (Simple pts) = map (uncurry Segment) pairs
    where pairs = pairwise (pts ++ [head pts])

promoteSimplePolygon :: SimplePolygon -> Polygon
promoteSimplePolygon sp = Polygon sp []

size :: Polygon -> Int
size (Polygon (Simple pts) holes) = length pts + sum (map (\(Simple pts) -> length pts) holes)
