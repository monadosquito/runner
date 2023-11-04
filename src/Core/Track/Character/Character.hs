{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}


module Core.Track.Character.Character where


import Core.Character.Character
import qualified Core.Track.Track as Track

import Control.Lens

import Core.Configuration.Configuration

import Control.Monad.Reader
import Numeric.Natural

import Core.Signal.Signal


data State = State { _hitPoints :: Natural
                   , _position :: Position
                   }


makeFieldsNoPrefix ''State


obstruct :: Position -> [[Track.Cell]] -> State -> State
obstruct _ _ state@(State 0 _) = state
obstruct (Position nextPosition) rows' state
    | rows' !! rowIndex !! columnIndex == Track.Obstacle
    = state & hitPoints -~ 1
    | otherwise
    = state & position .~ Position nextPosition
  where
    columnIndex = fromIntegral $ snd nextPosition
    rowIndex = fromIntegral $ fst nextPosition

revive :: Reader Options State
revive = do
    initialPosition <- spawn
    hitPoints' <- asks (_characterHitPoints)
    return $ State hitPoints' initialPosition

reflect :: Signal
        -> State
        -> Track.State
        -> Reader Configuration (State, Track.State)
reflect (FlowSignal Progress) previousChararacterState previousTrackState = do
    let nextCharacterPosition = progress
                              $ _position previousChararacterState
        nextCharacterState = obstruct nextCharacterPosition
                                      (Track._rows previousTrackState)
                                      previousChararacterState
        (nextRow, nextColumn) = nextCharacterState
                              ^. position
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
    return (nextCharacterState, nextTrackState)
  where
    (previousRow, previousColumn) = previousChararacterState
                                  ^. position
                                  . unPosition
                                  . to (bimap fromIntegral fromIntegral)
reflect (PlayerSignal signal) previousChararacterState previousTrackState = do
    let strafeSide = signalToSide signal
    nextCharacterPosition <- strafe strafeSide
                                    $ _position previousChararacterState
    let nextCharacterState = obstruct nextCharacterPosition
                                      (Track._rows previousTrackState)
                                      previousChararacterState
        (nextRow, nextColumn) = nextCharacterState
                              ^. position
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
    return (nextCharacterState, nextTrackState)
  where
    (previousRow, previousColumn) = previousChararacterState
                                  ^. position
                                  . unPosition
                                  . to (bimap fromIntegral fromIntegral)
