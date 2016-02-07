{-# LANGUAGE ScopedTypeVariables #-}
module Domain.Genetic where

import Algorithm.Genetic
import Domain.Types
import Domain.Coverage
import Visualization
import Control.Monad.Trans
import Control.Monad.Random
import Control.Monad.State.Strict (StateT, evalStateT)
import Control.Monad.State.Class
import Data.Coerce
import Geometry (Point(..), size)
import Data.Conduit
import qualified Data.Conduit.Combinators as Cond
import Data.List
import Data.Ord
import Common

type RandIO = StateT Int (RandT StdGen IO)

runRandIO :: RandIO a -> IO a
runRandIO r = do
    gen <- newStdGen
    evalRandT (evalStateT r 0) gen

mutateConf :: Configuration -> RandIO Configuration
mutateConf conf = do
    i <- getRandomR (0, length points - 1)
    dx <- getRandomR (-1, 1)
    dy <- getRandomR (-1, 1)
    return $! coerce $! take i points ++ [points !! i + Point dx dy] ++ drop (i + 1) points
    where points = coerce conf

crossConf :: Int -> Configuration -> Configuration -> RandIO Configuration
crossConf numChunks conf1 conf2 = do
    indices <- sort . take numChunks <$> getRandomRs (0, size')
    return $! coerce $! intertwine indices pts1 pts2
    where pts1 = coerce conf1 :: [Point]
          pts2 = coerce conf2
          size' = length pts1

evaluateConf :: ArtGallery -> Configuration -> RandIO Double
evaluateConf ag conf = case validate ag conf of
    Nothing  -> return 0
    Just att -> return $! evaluateCoverage att

withoutN :: Int -> [a] -> Int -> RandIO [a]
withoutN 0 list _   = return list
withoutN n list len = do
    i <- getRandomR (0, len - 1)
    withoutN (n - 1) (withoutI i list) (len - 1)

without :: Int -> [a] -> Source RandIO [a]
without n list = do
    yield (removeUniformly n list len)
    without' n list len
    where len = length list

without' :: Int -> [a] -> Int -> Source RandIO [a]
without' n list len = do
    lift (withoutN n list len) >>= yield
    without' n list len

runGallery :: ArtGallery -> RandIO Configuration
runGallery ag@(ArtGallery poly) = optimize (size poly) (size poly) ag (initialConfiguration ag)

logger :: ArtGallery -> Conduit [Evaluated Configuration] RandIO [Evaluated Configuration]
logger ag = do
    modify' (+ 1)
    i <- get
    maybeGen <- await
    case maybeGen of
        Just gen -> do
            liftIO $ print (i, fitness $ maximum gen)
            -- when (i `mod` 10 == 0) $ lift $ renderGen ag (show i) gen
            yield gen
            logger ag
        Nothing -> return ()

renderGen :: ArtGallery -> String -> Configuration -> RandIO ()
renderGen ag path conf = liftIO $ renderConfiguration ag conf ("out/" ++ path ++ ".png")

nubOrdBy :: (a -> a -> Ordering) -> [a] -> [a]
nubOrdBy f l = map head $ groupBy (\x y -> f x y == EQ) $ sortBy f l

filterSameAndDead :: [Evaluated Configuration] -> [Evaluated Configuration]
filterSameAndDead = nubOrdBy (comparing unit) . filter ((/= 0) . fitness)

optimize :: Int -> Int -> ArtGallery -> Configuration -> RandIO Configuration
optimize initSize cams ag conf = do
    let toRemove = max 1 $ (cams - floor (fromIntegral initSize * (2 / 5 :: Double))) `div` 10
    let nowCams = cams - toRemove
    confs <- without toRemove (coerce conf :: [Camera])
             =$= Cond.map Configuration
             =$= Cond.take (max 10 $ cams `div` toRemove)
             $$  Cond.sinkList

    let env = Environment { initial  = return confs
                          , mutate   = applyM 1 mutateConf
                          , cross    = crossConf (initSize `div` 10)
                          , evaluate = evaluateConf ag
                          , maxScore = 1
                          , filter'  = return . filterSameAndDead }
    res :: Maybe [Evaluated Configuration] <-
        runGenerations env =$= logger ag =$= Cond.take 100 $$ Cond.find ((>= 0.99) . fitness . head)
    case res of
        Just evals -> do
            let bestConf = unit (head evals)
            -- when (cams `mod` 10 == 0) $ renderGen ag ("sol" ++ show nowCams) bestConf
            liftIO $ putStrLn "Run done"
            liftIO $ putStrLn (show nowCams ++ " cams")
            if nowCams > 1 then optimize initSize (cams - toRemove) ag bestConf
            else do
                -- renderGen ag ("sol" ++ show nowCams) bestConf
                return bestConf
        Nothing -> do
            -- renderGen ag ("sol" ++ show cams) conf
            return conf
