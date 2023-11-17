{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}


module Core.Core where


import Core.Configuration.Configuration

import Core.Track.Character.Character
import Core.Track.Track

import Control.Lens
import Numeric.Natural

import Core.Character.Character
import Core.Signal.Signal

import Control.Monad.Reader


data CoreState = CoreState { _character :: CharacterState
                           , _score :: Natural
                           , _track :: TrackState
                           } deriving Eq


makeFieldsNoPrefix ''CoreState


reflect :: Monad m => Signal -> CoreState -> ReaderT Configuration m CoreState
reflect (FlowSignal Progress)
        (CoreState previousChararacterState previousScore previousTrackState)
    = do
    rowCrossingScoreBonus' <- asks (^. options . rowCrossingScoreBonus)
    let nextCharacterPosition = progress
                              $ _position previousChararacterState
        nextCharacterState = obstruct nextCharacterPosition
                                      (_rows previousTrackState)
                                      previousChararacterState
        nextScore = previousScore + rowCrossingScoreBonus'
        (nextRow, nextColumn) = nextCharacterState
                              ^. position
                              . unPosition
                              . to (bimap fromIntegral fromIntegral)
        nextTrackState = previousTrackState
                       & rows
                       . element previousRow
                       . element previousColumn
                       .~ TrailPart
                       & rows
                       . element nextRow
                       . element nextColumn
                       .~ Character
    return $ CoreState nextCharacterState nextScore nextTrackState
  where
    (previousRow, previousColumn) = previousChararacterState
                                  ^. position
                                  . unPosition
                                  . to (bimap fromIntegral fromIntegral)
reflect (PlayerSignal signal)
        (CoreState previousChararacterState previousScore previousTrackState)
    = do
    if | signal `elem` [StrafeLeft, StrafeRight] -> do
        let strafeSide = case signal of
                             StrafeLeft -> Left'
                             StrafeRight -> Right'
                             _ -> error "undefined side"
        nextCharacterPosition <- strafe strafeSide
                                        $ _position previousChararacterState
        let nextCharacterState = obstruct nextCharacterPosition
                                          (_rows previousTrackState)
                                          previousChararacterState
            (nextRow, nextColumn) = nextCharacterState
                                  ^. position
                                  . unPosition
                                  . to (bimap fromIntegral fromIntegral)
            nextTrackState = previousTrackState
                           & rows
                           . element previousRow
                           . element previousColumn
                           .~ TrailPart
                           & rows
                           . element nextRow
                           . element nextColumn
                           .~ Character
        return $ CoreState nextCharacterState previousScore nextTrackState
       | signal `elem` [SwingLeft, SwingRight] -> do
        enemyKillingScoreBonus' <- asks (^. options . enemyKillingScoreBonus)
        -- let swingSide = fromEnum $ signalToSide signal
        let swingSide = case signal of
                            SwingLeft -> -1
                            SwingRight -> 1
                            _ -> 0
            hitCell = previousColumn + swingSide
            hitObject = previousTrackState
                      ^? rows
                      . element previousRow
                      . element hitCell
            nextScore = case hitObject of
                            Just object | object /= kill object
                                ->
                                previousScore + enemyKillingScoreBonus'
                            _
                                ->
                                previousScore
            nextTrackState = previousTrackState
                           & rows
                           . element previousRow
                           . element hitCell
                           %~ kill
        return $ CoreState previousChararacterState nextScore nextTrackState
       | otherwise -> do
        return $ CoreState previousChararacterState
                           previousScore
                           previousTrackState
  where
    (previousRow, previousColumn) = previousChararacterState
                                  ^. position
                                  . unPosition
                                  . to (bimap fromIntegral fromIntegral)

initialiseCoreState :: Monad m
                    => TrackState
                    -> ReaderT Configuration m CoreState
initialiseCoreState trackState = do
    characterState <- revive
    let initialScore = 0
    return $ CoreState characterState initialScore trackState
