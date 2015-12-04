module Common where

import Data.List

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n xs = take (length xs + 1 - n) . map (take n) . tails $ xs

eps :: Double
eps = 2 ** (-32)
