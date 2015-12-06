module Visualization where

import Geometry
import Domain.Sample
import Domain.Coverage
import Domain.Types
import Diagrams (Diagram, P2, (#), Located)
import qualified Diagrams as Diag
import Diagrams.Backend.Rasterific (Rasterific)
import qualified Diagrams.Backend.Rasterific as Rast
import Data.Colour (Colour, withOpacity)
import qualified Data.Colour.Names as Color
import Data.Coerce

pointToP2 :: Point -> P2 Double
pointToP2 (Point x y) = Diag.p2 (x, y)

locate :: P2 Double -> Located (P2 Double)
locate p = p `Diag.at` Diag.p2 (0, 0)

drawPoint :: Point -> Diagram Rasterific
drawPoint (Point x y) = Diag.circle 0.05 # Diag.translate (Diag.r2 (x, y))
                                      # Diag.fc Color.red

drawPolygon :: Polygon -> Diagram Rasterific
drawPolygon (Polygon outer holes) = mconcat $ reverse $ drawSimplePolygon outer Color.black
                                                      : map (`drawSimplePolygon` Color.white) holes

drawSimplePolygon :: Diag.Color c => SimplePolygon -> c -> Diagram Rasterific
drawSimplePolygon (Simple pts) col =
    locPath # Diag.closeLine
            # Diag.strokeLoop
            # Diag.fillColor col
            # Diag.translate first
    where locPath = Diag.fromVertices (map pointToP2 pts)
          first   = head pts # \(Point x y) -> Diag.r2 (x, y)

drawAttempt :: Attempt -> Diagram Rasterific
drawAttempt (Attempt cfg ag) = cams `mappend` visDiags `mappend` drawPolygon (coerce ag)
    where visPolys = map (\pt -> visibilityPolygon pt (coerce ag)) (coerce cfg)
          visDiags = mconcat $ map (`drawSimplePolygon` withOpacity Color.yellow 0.1) visPolys
          cams     = mconcat $ map drawPoint (coerce cfg)

renderAttempt :: Attempt -> FilePath -> IO ()
renderAttempt att path = Rast.renderRasterific path (Diag.mkHeight 400) $ drawAttempt att

debugDiagram :: Diagram Rasterific -> IO ()
debugDiagram = Rast.renderRasterific "debug.png" (Diag.mkHeight 400)

debugAttempt :: Attempt -> IO ()
debugAttempt (Attempt conf gal) = debugDiagram $ dots `Diag.atop` baseDiag
    where baseDiag = drawPolygon $ coerce gal
          dots     = mconcat $ map drawPoint (coerce conf)
