{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}


module Core.State where


import Core.Configuration.Configuration

import qualified Core.Track.Character.Character as Character
import qualified Core.Track.Track as Track

import Control.Lens
import Numeric.Natural

import Core.Character.Character
import Core.Signal.Signal

import Control.Monad.Reader


data State = State { _character :: Character.State
                   , _score :: Natural
                   , _track :: Track.State
                   }


makeFieldsNoPrefix ''State


reflect :: Signal -> State -> Reader Configuration State
reflect (FlowSignal Progress)
        (State previousChararacterState previousScore previousTrackState)
    = do
    rowCrossingScoreBonus' <- asks (^. options . rowCrossingScoreBonus)
    let nextCharacterPosition = progress
                              $ Character._position previousChararacterState
        nextCharacterState = Character.obstruct nextCharacterPosition
                                                (Track._rows previousTrackState)
                                                previousChararacterState
        nextScore = previousScore + rowCrossingScoreBonus'
        (nextRow, nextColumn) = nextCharacterState
                              ^. Character.position
                              . unPosition
                              . to (bimap fromIntegral fromIntegral)
        nextTrackState = previousTrackState
                       & Track.rows
                       . element previousRow
                       . element previousColumn
                       .~ Track.TrailPart
                       & Track.rows
                       . element nextRow
                       . element nextColumn
                       .~ Track.Character
    return $ State nextCharacterState nextScore nextTrackState
  where
    (previousRow, previousColumn) = previousChararacterState
                                  ^. Character.position
                                  . unPosition
                                  . to (bimap fromIntegral fromIntegral)
reflect (PlayerSignal signal)
        (State previousChararacterState previousScore previousTrackState)
    = do
    if | signal `elem` [StrafeLeft, StrafeRight] -> do
        let strafeSide = case signal of
                             StrafeLeft -> Left'
                             StrafeRight -> Right'
                             _ -> error "undefined side"
        nextCharacterPosition <- strafe strafeSide
                                        $ Character._position previousChararacterState
        let nextCharacterState = Character.obstruct nextCharacterPosition
                                                    (Track._rows previousTrackState)
                                                    previousChararacterState
            (nextRow, nextColumn) = nextCharacterState
                                  ^. Character.position
                                  . unPosition
                                  . to (bimap fromIntegral fromIntegral)
            nextTrackState = previousTrackState
                           & Track.rows
                           . element previousRow
                           . element previousColumn
                           .~ Track.TrailPart
                           & Track.rows
                           . element nextRow
                           . element nextColumn
                           .~ Track.Character
        return $ State nextCharacterState previousScore nextTrackState
       | signal `elem` [SwingLeft, SwingRight] -> do
        enemyKillingScoreBonus' <- asks (^. options . enemyKillingScoreBonus)
        -- let swingSide = fromEnum $ signalToSide signal
        let swingSide = case signal of
                            SwingLeft -> -1
                            SwingRight -> 1
                            _ -> 0
            hitCell = previousColumn + swingSide
            hitObject = previousTrackState
                        ^? Track.rows
                        . element previousRow
                        . element hitCell
            nextScore = case hitObject of
                            Just object | object /= Character.kill object
                                ->
                                previousScore + enemyKillingScoreBonus'
                            _
                                ->
                                previousScore
            nextTrackState = previousTrackState
                           & Track.rows
                           . element previousRow
                           . element hitCell
                           %~ Character.kill
        return $ State previousChararacterState nextScore nextTrackState
       | otherwise -> do
        return $ State previousChararacterState previousScore previousTrackState
  where
    (previousRow, previousColumn) = previousChararacterState
                                  ^. Character.position
                                  . unPosition
                                  . to (bimap fromIntegral fromIntegral)
