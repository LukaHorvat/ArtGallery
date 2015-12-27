{-# LANGUAGE ViewPatterns #-}
module Domain.Parse where

import Geometry
import Data.List.Split
import Data.Coerce

parsePoly :: String -> SimplePolygon
parsePoly str = coerce $ map parsePoint $ chunksOf 2 coords
    where _ : coords = words str
          parsePoint [ splitOn "/" -> [read -> xd, read -> xn]
                     , splitOn "/" -> [read -> yd, read -> yn] ] = Point (xd / xn) (yd / yn)
          parsePoint _ = error "chunksOf 2 returned a list of something other than 2 elements"

polygonFromFile :: FilePath -> IO SimplePolygon
polygonFromFile path = parsePoly <$> readFile path
