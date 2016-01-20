{-# LANGUAGE ScopedTypeVariables #-}
module Domain.Genetic where

import Common
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

withoutEach :: [a] -> [[a]]
withoutEach list = map withoutN [0..length list - 1]
    where withoutN i = take i list ++ drop (i + 1) list

runGallery :: ArtGallery -> RandIO ()
runGallery ag@(ArtGallery poly) = optimize (size poly) ag (initialConfiguration ag)

logger :: ArtGallery -> Conduit [Evaluated Configuration] RandIO [Evaluated Configuration]
logger ag = do
    modify' (+ 1)
    i <- get
    maybeGen <- await
    case maybeGen of
        Just gen -> do
            liftIO $ print i
            -- when (i `mod` 10 == 0) $ lift $ renderGen ag (show i) gen
            yield gen
            logger ag
        Nothing -> return ()

renderGen :: ArtGallery -> String -> [Evaluated Configuration] -> RandIO ()
renderGen ag path gen = do
    let maybeAtt = validate ag (unit $ maximum gen)
    case maybeAtt of
        Just att -> liftIO $ renderAttempt att ("out/" ++ path ++ ".png")
        Nothing  -> liftIO $ putStrLn $ "Invalid configuration on generation " ++ path

optimize :: Int -> ArtGallery -> Configuration -> RandIO ()
optimize cams ag conf = do
    confs <- map Configuration <$> shuffle (withoutEach $ coerce conf)
    let env = Environment { initial  = return confs
                          , mutate   = mutateConf
                          , cross    = crossConf
                          , evaluate = evaluateConf ag
                          , maxScore = 1 }
    Just (res :: [Evaluated Configuration]) <-
        runGenerations env =$= logger ag $$ Cond.find ((>= 0.99) . fitness . head)
    renderGen ag ("sol" ++ show cams) res
    liftIO $ putStrLn "Run done"
    optimize (cams - 1) ag (unit $ head res)
