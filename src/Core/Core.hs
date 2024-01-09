{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}


module Core.Core where


import Core.Configuration.Configuration

import Core.Track.Character.Character
import Core.Track.Track

import Control.Lens
import Numeric.Natural

import Core.Character.Character
import Core.Signal.Signal

import Control.Monad.Reader
import Control.Lens.Extras
import Control.Monad.State


data CoreState = CoreState { _character :: CharacterState
                           , _score :: Natural
                           , _track :: TrackState
                           } deriving Eq


makeFieldsNoPrefix ''CoreState


reflect :: Monad m => Signal -> CoreState -> ReaderT Configuration m CoreState
reflect (FlowSignal Progress) core = do
    rowCrossingScoreBonus' <- asks (^. options . rowCrossingScoreBonus)
    return $ (`execState` core) $ do
        previousChararacterPosition <- use $ character . position
        (previousRow, previousColumn) <- use
                                      $ character
                                      . position
                                      . unPosition
                                      . to (bimap (fromIntegral @Natural @Int)
                                                  (fromIntegral @Natural @Int)
                                           )
        let nextCharacterPosition = progress previousChararacterPosition
        previousTrackRows <- use $ track . rows
        character %= obstruct nextCharacterPosition previousTrackRows
        (nextRow, nextColumn) <- use
                              $ character
                              . position
                              . unPosition
                              . to (bimap (fromIntegral @Natural @Int)
                                          (fromIntegral @Natural @Int)
                                   )
        nextCell <- gets (^? track . rows . ix nextRow . ix nextColumn)
        let berserkerBonusAheadChar = nextCell == Just BerserkerOrb
        when berserkerBonusAheadChar $ do
            character . superpower .= Just berserkerSuperpower
        track . rows . ix previousRow . ix previousColumn .= TrailPart
        track . rows . ix nextRow . ix nextColumn .= Character
        score += rowCrossingScoreBonus'
        currentSuperpower <- use $ character . superpower
        let berserkerSuperpowerOn = is (_Just . _BerserkerSuperpower)
                                       currentSuperpower
        score += toPickScoreBonus nextCell
        when berserkerSuperpowerOn $ do
            leftNextCell <- gets (^? track
                                  . rows
                                  . ix nextRow
                                  . ix (nextColumn - 1)
                                 )
            rightNextCell <- gets (^? track
                                   . rows
                                   . ix nextRow
                                   . ix (nextColumn + 1)
                                  )
            track . rows . ix nextRow . ix (nextColumn - 1) .= TrailPart
            track . rows . ix nextRow . ix (nextColumn + 1) .= TrailPart
            score += toDestructionScoreBonus leftNextCell
            score += toDestructionScoreBonus rightNextCell
        get
reflect (PlayerSignal signal) coreState = do
    conf <- ask
    enemyKillingScoreBonus' <- asks (^. options . enemyKillingScoreBonus)
    return $ (`execState` coreState) $ do
        (previousRow, previousColumn) <- use
                                      $ character
                                      . position
                                      . unPosition
                                      . to (bimap (fromIntegral @Natural @Int)
                                                  (fromIntegral @Natural @Int)
                                           )
        if | signal `elem` [StrafeLeft, StrafeRight] -> do
            let strafeSide = case signal of
                                 StrafeLeft -> Left'
                                 StrafeRight -> Right'
                                 _ -> error "undefined side"
            previousChararacterPosition <- use $ character . position
            previousTrackRows <- use $ track . rows
            let nextCharacterPosition = runReader (strafe strafeSide
                                                          previousChararacterPosition
                                                  )
                                                  conf
            character %= obstruct nextCharacterPosition previousTrackRows
            (nextRow, nextColumn) <- use
                                  $ character
                                  . position
                                  . unPosition
                                  . to (bimap (fromIntegral @Natural @Int)
                                              (fromIntegral @Natural @Int)
                                       )
            nextCell <- gets (^? track . rows . ix nextRow . ix nextColumn)
            let berserkerBonusAheadChar = nextCell == Just BerserkerOrb
            track . rows . ix previousRow . ix previousColumn .= TrailPart
            track . rows . ix nextRow . ix nextColumn .= Character
            when berserkerBonusAheadChar $ do
                character . superpower .= Just berserkerSuperpower
            score += toPickScoreBonus nextCell
           | signal `elem` [SwingLeft, SwingRight] -> do
            let swingSide = case signal of
                                SwingLeft -> -1
                                SwingRight -> 1
                                _ -> 0
                hitCell = previousColumn + swingSide
            hitObject <- gets (^? track . rows . ix previousRow . ix hitCell)
            let enemyKilled | Just object <- hitObject = object /= kill object
                            | otherwise = False
            when enemyKilled $ do
                track . rows . element previousRow . element hitCell %= kill
                score += enemyKillingScoreBonus'
           | otherwise ->
            return ()
        get

initialiseCoreState :: Monad m
                    => TrackState
                    -> ReaderT Configuration m CoreState
initialiseCoreState trackState = do
    characterState <- revive
    let initialScore = 0
    return $ CoreState characterState initialScore trackState
    
toDestructionScoreBonus :: Maybe Cell -> Natural
toDestructionScoreBonus (Just LivingEnemy) = 20
toDestructionScoreBonus _ = 0

toPickScoreBonus :: Maybe Cell -> Natural
toPickScoreBonus (Just BronzeCoin) = 5
toPickScoreBonus (Just GoldCoin) = 10
toPickScoreBonus _ = 0
