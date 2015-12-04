module Domain.Coverage where

import Geometry
import Common
import Domain.Types
import Domain.Initial
import Data.Coerce
import Debug.Trace

initialConfiguration :: ArtGallery -> Configuration
initialConfiguration (ArtGallery poly) = coerce $ initialPoints poly

validate :: ArtGallery -> Configuration -> Maybe Attempt
validate ag@(ArtGallery poly) conf
    | all ((== Inside) . (`isInPoly` poly)) points = Just $ Attempt conf ag
    | otherwise                                    = Nothing
    where points = coerce conf :: [Point]

initialAttempt :: ArtGallery -> Attempt
initialAttempt ag = Attempt (initialConfiguration ag) ag

generateVisibilities :: Attempt -> [SimplePolygon]
generateVisibilities (Attempt att (ArtGallery ag)) = map (`visibilityPolygon` ag) (coerce att)

evaluateCoverage :: Attempt -> Double
evaluateCoverage att@(Attempt _ (ArtGallery ag))
    | rounded > 1 = error $ "Visible area (" ++ show coveredArea
                         ++ ") greater than total area (" ++ show galleryArea ++ ")"
    | otherwise = rounded
    where coveredArea = unionArea $ map promoteSimplePolygon $ generateVisibilities att
          galleryArea = unionArea [ag]
          percentage  = coveredArea / galleryArea
          rounded     = (fromIntegral . round) (percentage * 1000) / 1000
