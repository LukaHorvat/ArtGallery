{-# LANGUAGE TupleSections #-}
module Geometry.Visibility where

import Common
import Geometry.Point
import Geometry.Polygon
import Geometry.Angle
import Data.List (minimumBy)
import Data.Ord
import Data.Maybe (mapMaybe)
import Data.Function (on)
import Stream
import Data.Map (Map)
import qualified Data.Map as Map

newtype Loop = Loop (Stream Segment)

instance Show Loop where
    show _ = "Loop"

loopSeg :: Loop -> Segment
loopSeg (Loop (s :> _)) = s

streamToLoops :: Stream Segment -> [Loop]
streamToLoops (x :> xs) = Loop (x :> xs) : streamToLoops xs

simpleLoops :: SimplePolygon -> [Loop]
simpleLoops (Simple pts) = take len $ streamToLoops $ cycleList segs
    where segs = map (\[x, y] -> Segment x y) $ slidingWindow 2 $ pts ++ [head pts]
          len  = length pts

data Corner = ToCW Point | ToCCW Point deriving (Eq, Ord, Read, Show)

startPoint :: Segment -> Point
startPoint (Segment x _) = x

simpleCorners :: Point -> SimplePolygon -> [(Corner, Loop)]
simpleCorners cam poly = map middle $ filter (not . ordered . map snd) windows
    where loops   = simpleLoops poly
          toTriple loop = let p = startPoint $ loopSeg loop in ((p, loop), polarAngle cam p)
          angled  = map toTriple loops
          windows = slidingWindow 3 $ last angled : angled ++ [head angled]
          ordered [x, y, z] = x `cwFrom` y && y `cwFrom` z || x `ccwFrom` y && y `ccwFrom` z
          ordered _         = error "slidingWindow 3 produces a list that isn't 3 long"
          middle [(_, ax), ((pt, loop), ay), _]
              | ax `cwFrom` ay = (ToCCW pt, loop)
              | otherwise      = (ToCW  pt, loop)
          middle _          = error "error in middle"

corners :: Point -> Polygon -> [(Corner, Loop)]
corners cam = concat . overSimple (simpleCorners cam)

polygonLoops :: Polygon -> [Loop]
polygonLoops = concat . overSimple simpleLoops

intersect :: Point -> Vector -> Segment -> Maybe Point
intersect p r (Segment q q')
    | t < -eps                  = Nothing
    | u >= -eps && u <= 1 + eps = Just (q + u `scale` s)
    | otherwise                 = Nothing
    where s = q `vecTo` q'
          t = ((q - p) `cross` s) / (r `cross` s)
          u = ((p - q) `cross` r) / (s `cross` r)

rayCast :: Point -> Vector -> [Loop] -> (Point, Loop)
rayCast start dir loops = minimumBy (comparing (sqrDist start . fst)) inters
    where inters = mapMaybe tryLoop loops
          tryLoop loop = (, loop) <$> intersect start dir (loopSeg loop)

validCast :: Point -> Vector -> (Point, Loop) -> Bool
validCast start dir (pt, _) = sqrDist start pt > sqrMag dir

type Ladders = Map Point (Point, Loop)
type Slides  = Map Segment [(Point, Loop)]

addSlide :: Segment -> (Point, Loop) -> Slides -> Slides
addSlide seg cast = Map.alter ins seg
    where ins Nothing  = Just [cast]
          ins (Just l) = Just (cast : l)

addRay :: Point -> [Loop] -> Ladders -> Slides
       -> (Corner, Loop) -> (Ladders, Slides)
addRay cam loops ladders slides (ToCCW pt, _)
    | validCast cam dir cast = (Map.insert pt cast ladders, slides)
    | otherwise = (ladders, slides)
    where dir  = rotate (cam `vecTo` pt) 0.0001
          cast = rayCast cam dir loops
addRay cam loops ladders slides (ToCW pt, contLoop)
    | validCast cam dir cast = (ladders, addSlide (loopSeg startLoop) (pt', contLoop) slides)
    | otherwise = (ladders, slides)
    where dir  = rotate (cam `vecTo` pt) (-0.0001)
          cast@(pt', startLoop) = rayCast cam dir loops

makeRayMaps :: Point -> Polygon -> (Ladders, Slides)
makeRayMaps cam poly = foldl (uncurry $ addRay cam loops) (Map.empty, Map.empty) $ corners cam poly
    where loops = polygonLoops poly

nextPointLoop :: Loop -> (Point, Loop)
nextPointLoop (Loop (_ :> segs@(Segment p _ :> _))) = (p, Loop segs)

traversePoly :: Angle -> Point -> (Point, Loop) -> Ladders -> Slides -> [Point]
traversePoly lastAng cam (pt, loop) ladders slides
    | lastAng > ang = []
    | otherwise     =
        case Map.lookup (loopSeg loop) slides of
            Just sli -> slide sli
            Nothing -> case Map.lookup pt ladders of
                Just lad -> pt : traversePoly ang cam lad ladders slides
                Nothing  -> pt : traversePoly ang cam (nextPointLoop loop) ladders slides
    where ang = polarAngle cam pt
          slide sli = pt : land : traversePoly ang cam (nextPointLoop nextLoop) ladders slides
              where (land, nextLoop) = minimumBy (compareRelative ang `on` toAng) sli
                    toAng (pt', _) = polarAngle cam pt'

visibilityPolygon :: Point -> Polygon -> SimplePolygon
visibilityPolygon start poly = Simple $! traversePoly 0 start right ladders slides
    where (ladders, slides) = makeRayMaps start poly
          loops = polygonLoops poly
          right = rayCast start (Point 1 0) loops
