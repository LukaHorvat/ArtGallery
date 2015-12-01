module Domain.Sample where

import Geometry
import Domain.Types

star :: Int -> Double -> SimplePolygon
star n rad = Simple $ concat $ zipWith (\i o -> [i, o]) inner outer
    where angleDelta = 2 * pi / fromIntegral n
          polar r phi = Point (r * cos phi) (r * sin phi)
          inner = map (\x -> polar rad (angleDelta * fromIntegral x)) [1..n]
          outer = map (\x -> polar (rad * 2) (angleDelta * fromIntegral x + angleDelta / 2)) [1..n]

polyStar :: Polygon
polyStar = Polygon (star 5 10) [star 5 5]

sampleGallery :: ArtGallery
sampleGallery = ArtGallery polyStar
