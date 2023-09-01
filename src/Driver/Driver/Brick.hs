{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}


module Driver.Driver.Brick where


import Core.Character.Character
import Core.Configuration.Configuration
import Core.Port.Driver
import Core.Track.Character.Character
import Core.Track.Configuration.Configuration
import Core.Track.Track

import Core.Script.Track

import Driver.Renderer.Cnsl

import Brick
import Brick.BChan
import Brick.Widgets.Center
import Control.Concurrent
import Control.Lens
import Control.Lens.Extras
import Control.Monad
import Control.Monad.Free
import Control.Monad.Reader
import Data.IORef
import System.Random
import qualified Data.Map.NonEmpty as Map
import qualified Graphics.Vty as Vty


data GlobState = GlobState { _currTrackCycPiecesCnt
                           , _currTrackCycRemRowsCnt :: Int
                           }

data LocState = LocState { _charPos :: Position
                         , _track :: State
                         }


makeFieldsNoPrefix ''LocState


data Brick
instance Driver Brick where
    run _ = do
        currTrackName <- asks (^. preferences . trackName)
        opts <- asks _options
        trackPieceCap <- asks (^. preferences
                               . trackPieceCapacity
                               . to fromIntegral
                              )
        (currTrackState, evChan, globStateRef) <- liftIO $ do
            gen <- newStdGen
            let currTrack = tracks' Map.! currTrackName
                currTrackCyc = getCycle currTrack
                currTrackInf = is _Free currTrackCyc
                interpret'' = configure opts
                currTrackState = interpret'' currTrack gen
                               & rows %~ reverse
                currTrackRowsCnt = currTrackState ^. rows . to length
                currTrackPiecesCnt = currTrackRowsCnt `div` trackPieceCap
                currTrackRemRowsCnt = currTrackRowsCnt
                                    `mod` trackPieceCap
                                    + currTrackPiecesCnt
                trackRowTime = round $ 1 / progressSpeed * 1000000
            evChan <- newBChan 10
            globStateRef <- newIORef initGlobState
            void . forkIO $ do
                replicateM_ currTrackPiecesCnt $ do
                    replicateM_ trackPieceCap $ do
                        threadDelay trackRowTime
                        writeBChan evChan FeedTrackRow
                    writeBChan evChan FeedTrackPiece
                    writeBChan evChan ReturnChar
                replicateM_ currTrackRemRowsCnt $ do
                    threadDelay trackRowTime
                    writeBChan evChan FeedTrackRow
                if currTrackInf
                then forever $ do
                    writeBChan evChan ReturnChar
                    writeBChan evChan FeedTrackCyc
                    GlobState {..} <- readIORef globStateRef
                    replicateM_ _currTrackCycPiecesCnt $ do
                        replicateM_ trackPieceCap $ do
                            threadDelay trackRowTime
                            writeBChan evChan FeedTrackRow
                        writeBChan evChan FeedTrackPiece
                        writeBChan evChan ReturnChar
                    replicateM_ (_currTrackCycRemRowsCnt + 1) $ do
                        threadDelay trackRowTime
                        writeBChan evChan FeedTrackRow
                else writeBChan evChan Fin
            return (currTrackState, evChan, globStateRef)
        app' <- app globStateRef
        initLocState' <- initLocState currTrackState
        liftIO $ do
            let bldVty = Vty.mkVty Vty.defaultConfig
            initVty <- bldVty
            void $ customMain initVty bldVty (Just evChan) app' initLocState'

data FlowEvent = FeedTrackRow
               | FeedTrackPiece
               | FeedTrackCyc
               | Fin
               | ReturnChar

app :: IORef GlobState -> ReaderT Configuration IO (App LocState FlowEvent ())
app globStateRef = do
    conf <- ask
    hndlEv' <- hndlEv globStateRef
    return $ App { appDraw = draw conf
                 , appChooseCursor = neverShowCursor
                 , appHandleEvent = hndlEv'
                 , appStartEvent = pure ()
                 , appAttrMap = const $ attrMap Vty.defAttr []
                 }

draw :: Configuration -> LocState -> [Widget ()]
draw conf (LocState charPos' trackState)
    =
    [center . str . show . RndredTrackLines $ reflect charPos' trackPiece]
  where
    trackPieceCap = conf ^. preferences . trackPieceCapacity . to fromIntegral
    trackPiece = trackState ^. rows . to (take trackPieceCap)

hndlEv :: IORef GlobState
       -> ReaderT Configuration
                  IO
                  (BrickEvent () FlowEvent -> EventM () LocState ())
hndlEv globStateRef = do
    currTrackName <- asks (^. preferences . trackName)
    opts <- asks _options
    trackPieceCap <- asks (^. preferences
                           . trackPieceCapacity
                           . to fromIntegral
                          )
    let interpretFrom' = configureFrom opts
    return $ \case
                 AppEvent FeedTrackRow -> do
                     charPos %= progress
                 AppEvent FeedTrackPiece -> do
                     track . rows %= drop (trackPieceCap - 1)
                 VtyEvent (Vty.EvKey k []) -> do
                     if | k == (Vty.KChar 'q') -> halt
                        | k `elem` [Vty.KChar 'a', Vty.KLeft]
                        -> charPos %= (`runReader` opts) . strafe Left'
                        | k `elem` [Vty.KChar 'd', Vty.KRight]
                        -> charPos %= (`runReader` opts) . strafe Right'
                        | otherwise -> return ()
                 AppEvent FeedTrackCyc -> do
                     gen <- newStdGen
                     prevTrackState <- use track
                     let currTrack = tracks' Map.! currTrackName
                         currTrackCyc = getCycle currTrack
                         initTrackState = prevTrackState & rows %~ (pure . head)
                         contTrackState = interpretFrom' initTrackState
                                                         currTrackCyc
                                                         gen
                     track .= contTrackState
                     track . rows %= reverse
                     let currTrackCycPiecesCnt = contTrackState
                                                 ^. rows
                                                 . to length
                                               `div` trackPieceCap
                         currTrackCycRemRowsCnt = contTrackState
                                                  ^. rows
                                                  . to length
                                                `mod` trackPieceCap
                                                + (currTrackCycPiecesCnt - 1)
                     liftIO $ do
                         writeIORef globStateRef
                                    $ GlobState currTrackCycRemRowsCnt
                                                currTrackCycPiecesCnt
                 AppEvent Fin -> do
                     halt
                 AppEvent ReturnChar -> do
                     charPos %= backtrack
                 _ -> do
                     return ()

initGlobState :: GlobState
initGlobState = GlobState 0 0

initLocState :: State -> ReaderT Configuration IO LocState
initLocState trackState = do
    opts <- asks _options
    let initCharPos = runReader spawn opts
    return $ LocState initCharPos (trackState & rows %~ reverse)
