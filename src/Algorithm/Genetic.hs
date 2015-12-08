{-# LANGUAGE RecordWildCards #-}
module Algorithm.Genetic where

import Control.Monad
import Control.Monad.Trans
import Data.Ord
import Data.List
import Data.Conduit

data Environment m a = Environment { initial  :: m [a]
                                   , mutate   :: a -> m a
                                   , cross    :: a -> a -> m a
                                   , evaluate :: a -> m Double }

data Evaluated a = Evaluated { fitness :: !Double
                             , unit    :: !a } deriving (Read, Show)

instance Ord (Evaluated a) where
    compare = comparing fitness

instance Eq (Evaluated a) where
    a == b = compare a b == EQ

evaluate' :: Monad m => (a -> m Double) -> a -> m (Evaluated a)
evaluate' eval x = (`Evaluated` x) <$> eval x

advance :: Monad m => Environment m a -> [Evaluated a] -> m [Evaluated a]
advance Environment{..} gen = do
    mutants <- mapM mutate top10
    crosses <- sequence $ cross <$> mutants <*> mutants
    mapM (evaluate' evaluate) $! top10 ++ mutants ++ crosses
    where sorted = sortBy (flip compare) gen
          top10  = map unit $ take 10 sorted

iterateM :: Monad m => (a -> m a) -> a -> Source m a
iterateM f x = do
    yield x
    iterateM f =<< lift (f x)

runGenerations :: Monad m => Environment m a -> Source m [Evaluated a]
runGenerations env@Environment{..} = do
    gen   <- lift initial
    evals <- lift $ forM gen $ \x -> (`Evaluated` x) <$> evaluate x
    iterateM (advance env) evals
