module Main where

import Geometry
import Domain.Sample
import Domain.Coverage
import Domain.Types
import Domain.Parse
import Control.Monad
import Visualization
import Algorithm.Sample
import Control.Monad.Random
import Algorithm.Genetic
import Data.Coerce
import System.Environment
import Data.List.Split

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

debug :: IO ()
debug = do
    poly <- polygonFromFile  "agp2009a-simplerand\\randsimple-20-1.pol"
    let gal = ArtGallery (Polygon poly [])
        att = initialAttempt gal
    print $ evaluateCoverage att
    renderAttempt att "debug.png"

-- main :: IO ()
-- main = do
--     res <- evalRandIO $ runNGenerations 1000 setup
--     print $ last res

main :: IO ()
main = do
    args <- getArgs
    case args of
        [gal, cams, out] -> draw gal cams out
        _           -> print "Usage: ./ArtGallery [polygon file] [camera file] [output file]"

draw :: FilePath -> FilePath -> FilePath -> IO ()
draw galPath camsPath outPath = do
    poly <- polygonFromFile galPath
    ptsString <- readFile camsPath
    let points = map (\[x, y] -> Point x y) $ chunksOf 2 $ map read $ words ptsString
    renderAttempt (Attempt (coerce points) (ArtGallery (Polygon poly []))) outPath
