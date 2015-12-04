{-# LANGUAGE ForeignFunctionInterface #-}
module Geometry.Clipping where

import Geometry.Point
import Geometry.Polygon
import Foreign.Marshal.Array
import Foreign.Ptr
import System.IO.Unsafe
import Foreign.C
import Data.Int
import Data.Coerce

foreign import ccall "wrapper.cpp UnionArea"
    unionAreaPtr :: Ptr CLLong -> Ptr CInt -> CInt -> IO CDouble

foreign import ccall "wrapper.cpp PointInPoly"
    pointInPolyPtr :: Ptr CLLong -> CInt -> CLLong -> CLLong -> IO CInt

unionAreaRaw :: [[Int64]] -> Double
unionAreaRaw polys = coerce longRes / 2 ** 60
    where numPolys = fromIntegral $ length polys
          sizes    = map ((`div` 2) . fromIntegral . length) polys
          buffer   = coerce $ concat polys
          longRes  = unsafePerformIO $
              withArray buffer $ \buffPtr ->
                  withArray sizes $ \sizesPtr ->
                      unionAreaPtr buffPtr sizesPtr numPolys

pointInPolyRaw :: [Int64] -> Int64 -> Int64 -> Int
pointInPolyRaw poly x y = fromIntegral $ unsafePerformIO $
    withArray (coerce poly) $ \buffPtr ->
        pointInPolyPtr buffPtr (fromIntegral $ length poly `div` 2) (coerce x) (coerce y)

type IsHole = Bool

pointToInts :: Point -> [Int64]
pointToInts (Point x y) = map round [x * 2 ** 30, y * 2 ** 30]

simpleToInts :: IsHole -> SimplePolygon -> [Int64]
simpleToInts isHole (Simple pts) = concatMap pointToInts pts'
    where pts' = if isHole then reverse pts else pts

polyToInts :: Polygon -> [[Int64]]
polyToInts (Polygon outer holes) = simpleToInts False outer : map (simpleToInts True) holes

unionArea :: [Polygon] -> Double
unionArea = unionAreaRaw . concatMap polyToInts

data PointPosition = Outside | Inside | OnBorder deriving (Eq, Ord, Read, Show)

oppositePosition :: PointPosition -> PointPosition
oppositePosition Outside  = Inside
oppositePosition Inside   = Outside
oppositePosition OnBorder = OnBorder

positionFromInt :: Int -> PointPosition
positionFromInt 0    = Outside
positionFromInt 1    = Inside
positionFromInt (-1) = OnBorder
positionFromInt i    = error $ "Integer " ++ show i ++ " not recognized as a valid point position"

isInSimplePoly :: IsHole -> Point -> SimplePolygon -> PointPosition
isInSimplePoly hole pt poly
    | hole      = oppositePosition posOnPoly
    | otherwise = posOnPoly
    where [x, y] = pointToInts pt
          posOnPoly = positionFromInt $ pointInPolyRaw (simpleToInts False poly) x y

isInPoly :: Point -> Polygon -> PointPosition
isInPoly pt (Polygon outer holes)
    | Outside `elem` positions  = Outside
    | OnBorder `elem` positions = OnBorder
    | otherwise                 = Inside
    where positions = isInSimplePoly False pt outer : map (isInSimplePoly True pt) holes
