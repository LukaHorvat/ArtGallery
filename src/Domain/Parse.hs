{-# LANGUAGE ScopedTypeVariables #-}
module Domain.Parse where

import Geometry
import Data.List.Split hiding (oneOf)
import Text.Megaparsec
import Text.Megaparsec.String

word :: Parser String
word = some (numberChar <|> oneOf "/-.") <* space

frac :: Parser Double
frac = do
    f <- word
    let [n :: Integer, d] = map read $ splitOn "/" f
    if abs (n * 100000000) < abs d then return 0
    else return (fromIntegral n / fromIntegral d)

point :: Parser Point
point = (\[x, y] -> Point x y) <$> count 2 frac

simple :: Parser SimplePolygon
simple = do
    n <- read <$> word
    pts <- count n point
    return (Simple pts)

polygon :: Parser Polygon
polygon = do
    outer <- simple
    n <- read <$> word <|> return 0
    holes <- count n simple
    return (Polygon outer holes)

cameras :: Parser [Point]
cameras = map (\[x, y] -> Point x y) . chunksOf 2 . map read <$> some word

run :: Parser a -> FilePath -> IO a
run parser path = do
    e <- runParser parser "" <$> readFile path
    case e of
        Left err -> error $ show err
        Right p  -> return p

polygonFromFile :: FilePath -> IO Polygon
polygonFromFile = run polygon

camerasFromFile :: FilePath -> IO [Point]
camerasFromFile = run cameras
