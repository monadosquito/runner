{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}


import Core.Port.Renderer

import Driver.Renderer.Cnsl

import Data.Proxy
import System.Random

import Core.Port.Environment

import Core.Script.Track

import Driver.Environment.Sys
import Driver.Parser.Aeson

import qualified Data.Map.NonEmpty as Map

import Core.Configuration.Configuration

import Core.Track.Configuration.Configuration

import Control.Concurrent
import Data.Foldable
import Control.Lens
import Control.Monad
import Control.Monad.Free
import Data.IORef

import Core.Track.Track


data FlowInput = FlowInput { trackCells :: [[Cell]]
                           , trackPiecesCnt
                           , trackPieceCap
                           , trackCycleLen
                           , trackCyclePiecesCnt :: Int
                           , trackRem :: [[Cell]]
                           , interpretFrom' :: GenerationState
                                            -> Track
                                            -> GenerationState
                           , trackGenState :: GenerationState
                           , trackGenStateRef :: IORef GenerationState
                           }


main :: IO ()
main = do
    gen <- newStdGen
    conf <- getConfiguration (Proxy @Sys) $ Proxy @Aeson
    run gen conf $ \FlowInput {..} -> do
        forM_ [0..trackPiecesCnt - 1] $ \ix' -> do
            rndrTrackPiece ix' trackPieceCap trackCells
            threadDelay 1000000
        render (Proxy @Cnsl) trackRem
        when (trackCycleLen > 0) . forever $ do
            forM_ [0..trackCyclePiecesCnt - 1] $ \ix' -> do
                trackCycle' <- genTrackCycle interpretFrom'
                                             trackGenState
                                             trackGenStateRef
                rndrTrackPiece ix' trackPieceCap trackCycle'
                threadDelay 1000000
            render (Proxy @Cnsl) trackRem
  where
    run gen conf flow = do
        let track = tracks' Map.! _trackName (_preferences conf)
            trackCells = rvsTrackCells
            interpret'' = configure $ _options conf
            trackPiecesCnt = length (_rows trackGenState) `div` trackPieceCap
            trackRem = drop (trackPieceCap * trackPiecesCnt) rvsTrackCells
            trackPieceCap = conf
                          ^. preferences
                          . trackPieceCapacity
                          . to fromIntegral
            interpretFrom' = configureFrom $ _options conf
            rvsTrackCells = trackGenState ^. rows . reversed
            trackGenState = interpret'' gen track
        let trackCycleLen = trackGenState
                          ^? cycle'
                          . _Free
                          . to (interpret gen . Free)
                          . rows
                          . to length
                          ^. non 0
            trackCyclePiecesCnt = trackCycleLen `div` trackPieceCap
        trackGenStateRef <- newIORef trackGenState
        flow FlowInput {..}
    rndrTrackPiece ix' cap rows' = do
        let trackPiece = take cap $ drop (ix' * cap) rows'
        render (Proxy @Cnsl) trackPiece
    genTrackCycle interpretFrom' trackGenState trackGenStateRef = do
        gen' <- newStdGen
        trackStartLine <- (^. rows . _head) <$> readIORef trackGenStateRef
        let contTrackGenState = trackGenState & rows .~ pure trackStartLine
                                              & generator .~ gen'
            newTrackGenState = interpretFrom' contTrackGenState
                                              $ _cycle' trackGenState
        writeIORef trackGenStateRef newTrackGenState
        return $ newTrackGenState ^. rows . _tail . reversed
