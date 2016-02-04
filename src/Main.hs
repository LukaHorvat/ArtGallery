{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-name-shadowing #-}
module Main where

import Geometry
import Domain.Sample
import Domain.Coverage
import Domain.Types
import Domain.Parse
import Domain.Genetic
import Control.Monad
import Visualization
import Algorithm.Sample
import Control.Monad.Random
import Algorithm.Genetic
import Data.Coerce
import System.Environment
import Data.List.Split
import Common

debug :: IO ()
debug = do
    poly <- polygonFromFile  "agp2009a-simplerand\\randsimple-20-1.pol"
    let gal = ArtGallery poly
        att = initialAttempt gal
    print $ evaluateCoverage att
    renderAttempt att "debug.png"

-- main :: IO ()
-- main = do
--     poly <- polygonFromFile "agp2009a-simplerand\\randsimple-100-1.pol"
--     cams <- camerasFromFile "agp2009a-simplerand_demo\\randsimple-100-1.pol.txt"
--     let config = coerce cams
--     let ag = ArtGallery poly
--     runRandIO $ optimize (length (coerce cams :: [Point])) ag config

main :: IO ()
main = do
    args <- getArgs
    (res, out) <- case args of
        [gal, out] -> do
            poly <- ArtGallery <$> polygonFromFile gal
            res <- runRandIO $ runGallery poly
            return (res, out)
        [gal, cams, out] -> do
            poly <- ArtGallery <$> polygonFromFile gal
            cams <- camerasFromFile cams
            res <- runRandIO $ optimize (size (coerce poly)) (length cams) poly (coerce cams)
            return (res, out)
        _ -> do
            putStrLn "Usage: ./ArtGallery <polygon file> [starting cameras] <output file>"
            error "Bad arguments"
    let coords = concatMap (\(Point x y) -> [x, y]) (coerce res :: [Point])
    writeFile out (unwords $ map show coords)

--
--
-- main :: IO ()
-- main = do
--     poly <- polygonFromFile "./gB-simple-simple/gB_simple-simple_25_100v-10h_2.pol"
--     -- cams <- camerasFromFile "agp2009a-simplerand_demo\\randsimple-100-1.pol.txt"
--     -- let config = coerce cams
--     let ag = ArtGallery poly
--     conf <- runRandIO $ runGallery ag
--     print conf

-- main :: IO ()
-- main = do
--     args <- getArgs
--     case args of
--         [gal, cams, out] -> draw gal cams out
--         _           -> print "Usag   e: ./ArtGallery [polygon file] [camera file] [output file]"
--
-- draw :: FilePath -> FilePath -> FilePath -> IO ()
-- draw galPath camsPath outPath = do
--     poly <- polygonFromFile galPath
--     ptsString <- readFile camsPath
--     let points = map (\[x, y] -> Point x y) $ chunksOf 2 $ map read $ words ptsString
--     renderAttempt (Attempt (coerce points) (ArtGallery (Polygon poly []))) outPath
