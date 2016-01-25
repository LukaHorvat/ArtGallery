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
import Control.Monad
import Data.List.Split

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

crossConf :: Configuration -> Configuration -> RandIO Configuration
crossConf conf1 conf2 = do
    i <- getRandomR (0, length pts1)
    return $! coerce $! take i pts1 ++ drop i pts2
    where pts1 = coerce conf1 :: [Point]
          pts2 = coerce conf2

evaluateConf :: ArtGallery -> Configuration -> RandIO Double
evaluateConf ag conf = case validate ag conf of
    Nothing  -> return 0
    Just att -> return $! evaluateCoverage att

iterateList :: Int -> (a -> [a]) -> a -> [a]
iterateList 0 _ x = [x]
iterateList n f x = f x >>= iterateList (n - 1) f

withoutI :: Int -> [a] -> [a]
withoutI i list = take i list ++ drop (i + 1) list

withoutN :: Int -> [a] -> Int -> RandIO [a]
withoutN 0 list _   = return list
withoutN n list len = do
    i <- getRandomR (0, len - 1)
    withoutN (n - 1) (withoutI i list) (len - 1)

removeUniformly :: Int -> [a] -> Int -> [a]
removeUniformly n list len = concatMap tail $ chunksOf ((len - 1) `div` n + 1) list

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
runGallery ag@(ArtGallery poly) = optimize (size poly) ag (initialConfiguration ag)

logger :: ArtGallery -> Conduit [Evaluated Configuration] RandIO [Evaluated Configuration]
logger ag = do
    modify' (+ 1)
    i <- get
    maybeGen <- await
    case maybeGen of
        Just gen -> do
            liftIO $ print (i, fitness $ head gen)
            -- when (i `mod` 10 == 0) $ lift $ renderGen ag (show i) gen
            yield gen
            logger ag
        Nothing -> return ()

renderGen :: ArtGallery -> String -> Configuration -> RandIO ()
renderGen ag path conf = liftIO $ renderConfiguration ag conf ("out/" ++ path ++ ".png")

optimize :: Int -> ArtGallery -> Configuration -> RandIO Configuration
optimize cams ag conf = do
    let toRemove = max 1 $ (cams - 200) `div` 10
    let nowCams = cams - toRemove
    confs <- without toRemove (coerce conf :: [Camera])
             =$= Cond.map Configuration
             =$= Cond.take (cams `div` toRemove)
             $$  Cond.sinkList

    let env = Environment { initial  = return confs
                          , mutate   = mutateConf
                          , cross    = crossConf
                          , evaluate = evaluateConf ag
                          , maxScore = 1 }
    res :: Maybe [Evaluated Configuration] <-
        runGenerations env =$= logger ag =$= Cond.take 30 $$ Cond.find ((>= 0.99) . fitness . head)
    case res of
        Just evals -> do
            let bestConf = unit (head evals)
            when (cams `mod` 10 == 0) $ renderGen ag ("sol" ++ show nowCams) bestConf
            liftIO $ putStrLn "Run done"
            liftIO $ putStrLn (show nowCams ++ " cams")
            if nowCams > 1 then optimize (cams - toRemove) ag bestConf
            else return bestConf
        Nothing -> do
            renderGen ag ("sol" ++ show nowCams) conf
            return conf
