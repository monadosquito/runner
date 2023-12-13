{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}


module Core.Track.Character.Character where


import Core.Character.Character
import Core.Track.Track

import Control.Lens

import Core.Configuration.Configuration

import Control.Monad.Reader
import Numeric.Natural


data CharacterState = CharacterState { _hitPoints :: Natural
                                     , _position :: Position
                                     }


makeFieldsNoPrefix ''CharacterState


obstruct :: Position -> [[Cell]] -> CharacterState -> CharacterState
obstruct _ _ state@(CharacterState 0 _) = state
obstruct (Position nextPosition) rows' state
    | isObstacle $ rows' !! rowIndex !! columnIndex = state & hitPoints -~ 1
    | otherwise
    = state & position .~ Position nextPosition
  where
    columnIndex = fromIntegral $ snd nextPosition
    rowIndex = fromIntegral $ fst nextPosition

revive :: Reader Options CharacterState
revive = do
    initialPosition <- spawn
    hitPoints' <- asks _characterHitPoints
    return $ CharacterState hitPoints' initialPosition

isObstacle :: Cell -> Bool
isObstacle LivingEnemy = True
isObstacle Obstacle = True
isObstacle _ = False

kill :: Cell ->  Cell
kill LivingEnemy = DeadEnemy
kill cell = cell
