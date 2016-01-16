module Common where

import Data.List
import Control.Monad
import Control.Monad.Random
import Data.Array.ST
import GHC.Arr

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
