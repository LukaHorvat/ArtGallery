module Common where

import Data.List
import Control.Monad
import Control.Monad.Random
import Data.Array.ST
import GHC.Arr
import Data.List.Split

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n xs = take (length xs + 1 - n) . map (take n) . tails $ xs

eps :: Double
eps = 2 ** (-32)

shuffle :: MonadRandom m => [a] -> m [a]
shuffle xs = do
    let l = length xs
    rands <- take l `fmap` getRandomRs (0, l-1)
    let ar' = runSTArray $ do
            ar <- thawSTArray $ listArray (0, l-1) xs
            forM_ (zip [0..(l-1)] rands) $ \(i, j) -> do
                vi <- readSTArray ar i
                vj <- readSTArray ar j
                writeSTArray ar j vi
                writeSTArray ar i vj
            return ar
    return (elems ar')

applyM :: Monad m => Int -> (a -> m a) -> a -> m a
applyM 0 _ m = return m
applyM n f m = f m >>= applyM (n - 1) f

iterateList :: Int -> (a -> [a]) -> a -> [a]
iterateList 0 _ x = [x]
iterateList n f x = f x >>= iterateList (n - 1) f

withoutI :: Int -> [a] -> [a]
withoutI i list = take i list ++ drop (i + 1) list

removeUniformly :: Int -> [a] -> Int -> [a]
removeUniformly n list len = concatMap tail $ chunksOf ((len - 1) `div` n + 1) list

intertwine :: [Int] -> [a] -> [a] -> [a]
intertwine is as bs = concat $! intertwine' lens as bs
    where lens = zipWith subtract (0 : is) is
          intertwine' []       as' _   = [as']
          intertwine' (l : ls) as' bs' = take l as' : intertwine' ls (drop l bs') (drop l as')
