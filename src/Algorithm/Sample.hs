module Algorithm.Sample where

import Algorithm.Genetic
import Control.Monad.Random (Rand, StdGen)
import qualified Control.Monad.Random as Rand

eval :: Double -> Rand StdGen Double
eval x = return $ 10 * sin (2 * x) - (x - 100) ** 2 - 0.1 * x ** 4

setup :: Environment (Rand StdGen) Double
setup = Environment { initial  = take 10 <$> Rand.getRandoms
                    , mutate   = \x -> (x +) <$> Rand.getRandomR (-5, 5)
                    , cross    = \x y -> do ex <- eval x
                                            ey <- eval y
                                            return $! (ex * x + ey * y) / (ex + ey)
                    , evaluate = eval
                    , maxScore = 10000
                    , filter'  = return}
