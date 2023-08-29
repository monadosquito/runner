{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}


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
import Core.Character.Character
import Core.Signal.Signal
import Core.Track.Character.Character

import Control.Monad.Reader
import System.Exit


data FlowInput = FlowInput { trackCells :: [[Cell]]
                           , trackPiecesCnt
                           , trackPieceCap
                           , trackCycleLen
                           , trackCyclePiecesCnt
                           , trackCycleRemRowsCnt
                           , trackRemRowsCnt :: Int
                           , interpretFrom' :: State -> Track -> StdGen -> State
                           , trackGenState :: State
                           , trackGenStateRef :: IORef State
                           }

data FlowState = FlowState { _trackPieceIx :: Int
                           , _flowThreadId :: Maybe ThreadId
                           }

data State' = State' { _pos :: Position
                     , _flow :: FlowState
                     }


makeFieldsNoPrefix ''FlowState
makeFieldsNoPrefix ''State'


main :: IO ()
main = do
    gen <- newStdGen
    conf <- getConfiguration (Proxy @Sys) $ Proxy @Aeson
    let track' = tracks' Map.! _trackName (_preferences conf)
        trackInfTail = getCycle track'
    let pos' = runReader spawn $ _options conf
    stateRef <- newIORef $ State' pos' (FlowState 0 Nothing)
    run gen conf $ \FlowInput {..} -> do
        flowThreadId' <- forkIO $ do
            forM_ [0..trackPiecesCnt - 1] $ \ix' -> do
                modifyIORef stateRef (& flow . trackPieceIx .~ ix')
                replicateM_ trackPieceCap $ do
                    State' coreState _ <- readIORef stateRef
                    rndrTrackPiece ix'
                                   trackPieceCap
                                   $ reflect coreState trackCells
                    modifyIORef stateRef (& pos %~ progress)
                    threadDelay oneTrackLineTime
            replicateM_ trackRemRowsCnt $ do
                State' coreState _ <- readIORef stateRef
                rndrTrackPiece trackPiecesCnt
                               trackPieceCap
                               $ reflect coreState trackCells
                modifyIORef stateRef (& pos %~ progress)
                threadDelay oneTrackLineTime
            when (trackCycleLen > 0) . forever $ do
                modifyIORef stateRef (& pos %~ backtrack)
                forM_ [0..trackCyclePiecesCnt - 1] $ \ix' -> do
                    trackCycle' <- genTrackCycle interpretFrom'
                                                 trackGenState
                                                 trackGenStateRef
                                                 trackInfTail
                    modifyIORef stateRef (& flow . trackPieceIx .~ ix')
                    replicateM_ trackPieceCap $ do
                        State' coreState _ <- readIORef stateRef
                        let updCells = reflect coreState trackCycle'
                        rndrTrackPiece ix' trackPieceCap updCells
                        modifyIORef stateRef (& pos %~ progress)
                        threadDelay oneTrackLineTime
                    modifyIORef stateRef (& pos %~ backtrack)
                replicateM_ trackCycleRemRowsCnt $ do
                    State' coreState _ <- readIORef stateRef
                    let updCells = reflect coreState trackCells
                    rndrTrackPiece trackPiecesCnt trackPieceCap updCells
                    modifyIORef stateRef (& pos %~ progress)
                    threadDelay oneTrackLineTime
        modifyIORef stateRef (& flow . flowThreadId .~ Just flowThreadId')
        forever $ do
            sigs <- getSignals $ Proxy @Sys
            case sigs of
                Just Quit -> do
                    state <- readIORef stateRef
                    case state ^. flow . flowThreadId of
                        Just flowThreadId'' -> killThread flowThreadId''
                        Nothing -> exitWith $ ExitFailure 1
                    exitSuccess
                Just (StrafeCharacter side) -> do
                    State' playerPos _ <- readIORef stateRef
                    modifyIORef stateRef
                                (& pos .~ runReader (strafe playerPos side)
                                                    (_options conf)

                                )
                Nothing -> return ()
  where
    run gen conf flow' = do
        let track' = tracks' Map.! _trackName (_preferences conf)
            trackCells = rvsTrackCells
            interpret'' = configure $ _options conf
            trackPiecesCnt = length (trackGenState ^. rows)
                           `div` trackPieceCap
            trackPieceCap = conf
                          ^. preferences
                          . trackPieceCapacity
                          . to fromIntegral
            interpretFrom' = configureFrom $ _options conf
            rvsTrackCells = trackGenState ^. rows . reversed
            trackGenState = interpret'' track' gen
            trackInfTail = getCycle track'
            trackCycleLen = trackInfTail
                          ^? _Free
                          . to ((`interpret''` gen) . Free)
                          . rows
                          . to length
                          ^. non 0
            trackCyclePiecesCnt = trackCycleLen `div` trackPieceCap
            trackCycleRemRowsCnt = length trackCells `mod` trackPieceCap
            trackRemRowsCnt = length trackCells `mod` trackPieceCap
        trackGenStateRef <- newIORef trackGenState
        flow' FlowInput {..}
    rndrTrackPiece ix' cap rows' = do
        let trackPiece = take cap $ drop (ix' * cap) rows'
        render (Proxy @Cnsl) trackPiece
    genTrackCycle interpretFrom' trackGenState trackGenStateRef trackInfTail = do
        gen' <- newStdGen
        trackStartLine <- (^. rows . _head)
                       <$> readIORef trackGenStateRef
        let contTrackGenState = trackGenState & rows .~ pure trackStartLine
            newTrackGenState = interpretFrom' contTrackGenState
                                              trackInfTail
                                              gen'
        writeIORef trackGenStateRef newTrackGenState
        return $ newTrackGenState ^. rows . _tail . reversed
    oneTrackLineTime = round $ 1 / progressSpeed * 1000000
