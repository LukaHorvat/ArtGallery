{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Geometry.Polygon where

import Geometry.Point
import GHC.Generics
import Data.Monoid
import Data.Hashable

data    Segment       = Segment Point Point deriving (Eq, Ord, Read, Show)
newtype SimplePolygon = Simple [Point]
                        deriving (Eq, Ord, Read, Show, Generic, Hashable)
data    Polygon       = Polygon SimplePolygon [SimplePolygon]
                        deriving (Eq, Ord, Read, Show, Generic, Hashable)
type IsHole = Bool

pairwise :: [a] -> [(a, a)]
pairwise [] = []
pairwise xs = zip xs (tail xs)

segments :: SimplePolygon -> [Segment]
segments (Simple pts) = map (uncurry Segment) pairs
    where pairs = pairwise (pts ++ [head pts])

promoteSimplePolygon :: SimplePolygon -> Polygon
promoteSimplePolygon sp = Polygon sp []

size :: Polygon -> Int
size (Polygon (Simple pts) holes) = length pts + sum (map (\(Simple x) -> length x) holes)

overSimple :: (SimplePolygon -> a) -> Polygon -> [a]
overSimple f (Polygon outer holes) = f outer : map (f . reverseSimple) holes

reverseSimple :: SimplePolygon -> SimplePolygon
reverseSimple (Simple pts) = Simple $! reverse pts

data Winding = CW | CCW deriving (Eq, Ord, Read, Show)

winding :: SimplePolygon -> Winding
winding poly
    | area >= 0 = CW
    | otherwise = CCW
    where area = foldMap segArea $ segments poly
          segArea (Segment (Point x1 y1) (Point x2 y2)) = Sum ((x2 - x1) * (y2 + y1))

rewind :: IsHole -> SimplePolygon -> SimplePolygon
rewind hole poly
    | not hole && initialWind == CW || hole && initialWind == CCW = reverseSimple poly
    | otherwise = poly
    where initialWind = winding poly
