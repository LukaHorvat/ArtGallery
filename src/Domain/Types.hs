module Domain.Types where

import Geometry

newtype Camera = Camera Point deriving (Eq, Ord, Read, Show)
newtype Configuration = Configuration [Camera] deriving (Eq, Ord, Read, Show)
newtype ArtGallery = ArtGallery Polygon deriving (Eq, Ord, Read, Show)
data Attempt = Attempt Configuration ArtGallery deriving (Eq, Ord, Read, Show)

makeArtGallery :: Polygon -> ArtGallery
makeArtGallery (Polygon outer holes) = ArtGallery rewound
    where rewound = Polygon (rewind False outer) (map (rewind True) holes)
