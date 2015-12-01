module Geometry.Point where

data Point = Point Double Double deriving (Eq, Ord, Read, Show)

type Vector = Point

instance Num Point where
    (Point x1 y1) + (Point x2 y2) = Point (x1 + x2) (y1 + y2)
    (Point x1 y1) * (Point x2 y2) = Point (x1 * x2) (y1 * y2)
    abs = error "abs not defined for points"
    signum = error "signum not defined for points"
    fromInteger = error "fromInteger not defined for points"
    negate (Point x y) = Point (-x) (-y)

vecTo :: Point -> Point -> Vector
vecTo p1 p2 = p2 - p1

direction :: Vector -> Double
direction (Point x y) = atan2 y x

sqrDist :: Point -> Point -> Double
sqrDist (Point x1 y1) (Point x2 y2) = (x2 - x1) ** 2 + (y2 - y1) ** 2

cross :: Vector -> Vector -> Double
cross (Point x1 y1) (Point x2 y2) = x1 * y2 - y1 * x2

scale :: Double -> Vector -> Vector
scale a (Point x y) = Point (a * x) (a * y)

rotate :: Vector -> Double -> Vector
rotate (Point x y) a = Point (cos a * x - sin a * y) (sin a * x + cos a * y)

jitter :: Vector -> [Vector]
jitter v = [rotate v (-0.0001), v, rotate v 0.0001]

magnitude :: Vector -> Double
magnitude (Point x y) = sqrt $ x * x + y * y

normalize :: Vector -> Vector
normalize vec = (1 / magnitude vec) `scale` vec
