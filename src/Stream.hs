{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Stream where

data Stream a = a :> Stream a deriving (Functor, Foldable, Traversable)

cycleList :: [a] -> Stream a
cycleList list = go inf
    where inf = cycle list
          go (x : xs) = x :> go xs
          go _        = error "Can't make a stream out of an empty list"

streamToList :: Stream a -> [a]
streamToList (x :> xs) = x : streamToList xs
