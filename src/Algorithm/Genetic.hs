{-# LANGUAGE RecordWildCards #-}
module Algorithm.Genetic where

import Control.Monad.Trans
import Data.Ord
import Data.List
import Data.Conduit

data Environment m a = Environment { initial  :: m [a]
                                   , mutate   :: a -> m a
                                   , cross    :: a -> a -> m a
                                   , evaluate :: a -> m Double
                                   , filter'  :: [Evaluated a] -> m [Evaluated a]
                                   , maxScore :: Double }

data UnitSource = Initial | Mutation | Crossing deriving (Eq, Ord, Read, Show)

data Evaluated a = Evaluated { fitness :: Double
                             , unit    :: !a
                             , source  :: UnitSource } deriving (Read, Show)

instance Ord (Evaluated a) where
    compare = comparing fitness

instance Eq (Evaluated a) where
    a == b = compare a b == EQ

evaluate' :: Monad m => UnitSource -> (a -> m Double) -> a -> m (Evaluated a)
evaluate' s eval x = (\v -> Evaluated v x s) <$> eval x

advance :: Monad m => Environment m a -> [Evaluated a] -> m [Evaluated a]
advance Environment{..} gen = do
    mutants <- mapM mutate top10
    crosses <- sequence $ cross <$> mutants <*> mutants
    new <- sequence $! map (evaluate' Initial evaluate) top10
                    ++ map (evaluate' Mutation evaluate) mutants
                    ++ map (evaluate' Crossing evaluate) crosses
    filter' new
    where sorted = sortBy (flip compare) gen
          top10  = map unit $ take 10 sorted

iterateM :: Monad m => (a -> m a) -> a -> Source m a
iterateM f x = do
    yield x
    iterateM f =<< lift (f x)

mapUntilMax :: Monad m => Double -> (a -> m (Evaluated a)) -> [a] -> m [Evaluated a]
mapUntilMax _  _ []       = return []
mapUntilMax mx f (x : xs) = do
    ev@(Evaluated fit _ _) <- f x
    if fit >= mx then return [ev]
    else (ev :) <$> mapUntilMax mx f xs

runGenerations :: Monad m => Environment m a -> Source m [Evaluated a]
runGenerations env@Environment{..} = do
    gen   <- lift initial
    evals <- lift $ mapUntilMax maxScore (evaluate' Initial evaluate) gen
    iterateM (advance env) evals
