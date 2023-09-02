{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}


import Core.Port.Renderer

import Driver.Renderer.Cnsl

import Data.Proxy
import System.Random
import qualified Data.List.NonEmpty as List

import Core.Port.Environment
import Core.Port.Parser

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
import qualified Lens.List.NonEmpty as List

import Core.Track.Track


data FlowInput = FlowInput { trackCells :: Maybe (List.NonEmpty [Cell])
                           , trackPiecesCnt
                           , trackPieceCap
                           , trackCycleLen
                           , trackCyclePiecesCnt :: Int
                           , trackRem :: Maybe (List.NonEmpty [Cell])
                           , interpretFrom' :: GenerationState
                                            -> Track
                                            -> GenerationState
                           , trackGenState :: GenerationState
                           , trackGenStateRef :: IORef GenerationState
                           }


main :: IO ()
main = do
    gen <- newStdGen
    conf <- getConfiguration (Proxy @Sys) . parseConfiguration $ Proxy @Aeson
    run gen conf $ \FlowInput {..} -> do
        forM_ [0..trackPiecesCnt - 1] $ \ix' -> do
            rndrTrackPiece ix' trackPieceCap trackCells
            threadDelay 1000000
        rndr trackRem
        when (trackCycleLen > 0) . forever $ do
            forM_ [0..trackCyclePiecesCnt - 1] $ \ix' -> do
                trackCycle' <- genTrackCycle interpretFrom'
                                             trackGenState
                                             trackGenStateRef
                rndrTrackPiece ix' trackPieceCap trackCycle'
                threadDelay 1000000
            rndr trackRem
  where
    run gen conf flow = do
        let track = tracks' Map.! _trackName (_preferences conf)
            trackCells = Just rvsTrackCells
            interpret'' = configure $ _options conf
            trackPiecesCnt = List.length (_cells trackGenState)
                           `div` trackPieceCap
            trackRem = List.nonEmpty
                     $ List.drop (trackPieceCap * trackPiecesCnt)
                                 rvsTrackCells
            trackPieceCap = conf
                          ^. preferences
                          . trackPieceCapacity
                          . to fromIntegral
            interpretFrom' = configureFrom $ _options conf
            rvsTrackCells = trackGenState ^. cells . reversed
            trackGenState = interpret'' gen track
        let trackCycleLen = trackGenState
                          ^? cycle'
                          . _Free
                          . to (interpret gen . Free)
                          . cells
                          . to List.length
                          ^. non 0
            trackCyclePiecesCnt = trackCycleLen `div` trackPieceCap
        trackGenStateRef <- newIORef trackGenState
        flow FlowInput {..}
    rndr (Just trackPiece) = render (Proxy @Cnsl) trackPiece
    rndr Nothing = pure ()
    rndrTrackPiece ix' cap (Just cells') = do
        let trackPiece = List.nonEmpty . take cap $ List.drop (ix' * cap) cells'
        rndr trackPiece
    rndrTrackPiece _ _ Nothing = return ()
    genTrackCycle interpretFrom' trackGenState trackGenStateRef = do
        gen' <- newStdGen
        trackStartLine <- (^. cells . List._head)
                       <$> readIORef trackGenStateRef
        let contTrackGenState = trackGenState & cells .~ pure trackStartLine
                                              & generator .~ gen'
            newTrackGenState = interpretFrom' contTrackGenState
                                              $ _cycle' trackGenState
        writeIORef trackGenStateRef newTrackGenState
        return $ newTrackGenState
               ^. cells
               . List._tail
               . reversed
               . to List.nonEmpty
