{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}


module Core.Script.Flow.Flow1 where


import Core.Configuration.Configuration
import Core.Flow.Flow
import Core.Signal.Signal
import Core.Track.Track

import Core.Script.Track

import Control.Lens
import Control.Lens.Extras
import Control.Monad.Free
import Control.Monad.Reader
import Data.Map
import Numeric.Natural


flow1 :: Monad m => ReaderT Configuration m (Flow r ())
flow1 = do
    characterProgressSpeed' <- asks (^. options . characterProgressSpeed)
    trackPartCapacity' <- asks (^. preferences
                                . trackPieceCapacity
                                . to (fromIntegral @Natural @Int)
                               )
    let frozen FlowState {..} = _characterStuck || _paused
        trackRowCrossTime = round $ 1 / characterProgressSpeed' * 1000000
        progress = wait trackRowCrossTime `until'` (not . frozen) $ do
            wait trackRowCrossTime
            dispatch $ Signal (FlowSignal Progress)
        struggle = do
            do
                FlowState {..} <- readState
                replicate' _characterHitsCount progress
                modifyState (& characterHitsCount .~ 0)
            `until'` ((== 0) . _characterHitsCount) $ return ()
        passTrackBody = do
            FlowState {..} <- readState
            let trackPartsCount = _trackRowsCount `div` trackPartCapacity'
                withSpareTrackRowsCount = _trackRowsCount + trackPartsCount
                withSpareTrackPartsCount = withSpareTrackRowsCount
                                         `div` trackPartCapacity'
                trackRemainderCount = withSpareTrackRowsCount
                                    `mod` trackPartCapacity'
                                    + trackPartsCount
            wait 1000000
            replicate' withSpareTrackPartsCount $ do
                wait 1000000
                replicate' (trackPartCapacity' - 1) progress
                struggle
                wait 1000000
                dispatch FeedNextTrackPiece
                dispatch ReturnCharacter
                modifyState (& characterHitsCount .~ 0)
            replicate' trackRemainderCount progress
            struggle
            dispatch ReturnCharacter
        passTrackTail = forever' $ do
            generator' <- makeGenerator
            dispatch $ FeedNextTrackCycle generator'
            wait 300000
            passTrackBody
        passTrack = do
            passTrackBody
            wait 1000000
            FlowState {..} <- readState
            let track' = tracks' ! _trackName
                trackCycle = getCycle track'
                trackHasTail = is _Free trackCycle
            when trackHasTail passTrackTail
    return $ do
        wait 100000
        flowThreadId' <- fork $ do
            forever $ wait 10000 `until'` (not . _started) $ do
                FlowState {..} <- readState
                maybe (return ()) killThread' _sessionThreadId
                generator' <- makeGenerator
                dispatch $ Refresh generator'
                wait 200000
                nextSessionThreadId <- fork passTrack
                wait 300000
                modifyState $ (& characterStuck .~ False)
                              . (& characterHitsCount .~ 0)
                              . (& started .~ True)
                              . (& paused .~ False)
                              . (& sessionThreadId .~ Just nextSessionThreadId)
        wait 200000
        modifyState (& flowThreadId .~ Just flowThreadId')
