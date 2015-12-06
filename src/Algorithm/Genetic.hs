{-# LANGUAGE RecordWildCards, BangPatterns #-}
module Algorithm.Genetic where

import Control.Monad
import Data.Ord
import Data.List

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
    
iterateM :: Monad m => Int -> (a -> m a) -> a -> m [a]
iterateM 0  _   _        = return []
iterateM n next !current = do
    x <- next current
    (current :) <$> iterateM (n - 1) next x

evaluate' :: Monad m => (a -> m Double) -> a -> m (Evaluated a)
evaluate' eval x = (`Evaluated` x) <$> eval x

advance :: Monad m => Environment m a -> [Evaluated a] -> m [Evaluated a]
advance Environment{..} gen = do
    mutants <- mapM mutate top10
    crosses <- sequence $ cross <$> mutants <*> mutants
    mapM (evaluate' evaluate) $! top10 ++ mutants ++ crosses
    where sorted = sortBy (flip compare) gen
          top10  = map unit $ take 10 sorted

runNGenerations :: Monad m => Int -> Environment m a -> m [[a]]
runNGenerations n env@Environment{..} = do
    gen <- initial
    evals <- forM gen $ \x -> (`Evaluated` x) <$> evaluate x
    map (map unit) <$> iterateM n (advance env) evals
