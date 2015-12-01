module Main where

import Geometry
import Domain.Sample
import Domain.Coverage
import Visualization

-- visPoly :: Double -> Double -> SimplePolygon
-- visPoly x y = visibilityPolygon (Point x y) polyStar
--
-- testDiag :: Double -> Double -> Diagram Rasterific
-- testDiag x y = Diag.circle 1 # Diag.translate (Diag.r2 (x, y)) # Diag.fc Color.red `Diag.atop` visDiag
--     where visDiag = drawPolygon polyStar `Diag.beneath` drawSimplePolygon (visPoly x y) Color.yellow
--
-- animation :: [Diagram Rasterific]
-- animation = map (uncurry testDiag . polar) [0, pi / 40..2 * pi]
--     where polar a = (rad a * cos a, rad a * sin a)
--           rad   a = (sin (a * 5 - pi / 2) + 1) * 3 + 7

-- main :: IO ()
-- main = mapM_ (uncurry render) indexedFrames
--     where indexedFrames = zip animation [(1 :: Int)..]
--           render diag i = Rast.renderRasterific ("out/frame" ++ show i ++ ".png") (Diag.mkHeight 400) diag

main :: IO ()
main = do
    let poly1 = Polygon ( Simple [Point 0 2, Point 4 2, Point 4 6, Point 0 6] )
                        [ Simple [Point 1 3, Point 3 3, Point 3 5, Point 1 5] ]
        poly2 = Polygon ( Simple [Point 2 0, Point 6 0, Point 6 4, Point 2 4] )
                        [ Simple [Point 3 1, Point 5 1, Point 5 3, Point 3 3] ]
    print $ unionArea [poly1, poly2]
