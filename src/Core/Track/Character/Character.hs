{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}


module Core.Track.Character.Character where


import Core.Character.Character
import qualified Core.Track.Track as Track

import Control.Lens

import Core.Configuration.Configuration

import Control.Monad.Reader
import Numeric.Natural


data State = State { _hitPoints :: Natural
                   , _position :: Position
                   }


makeFieldsNoPrefix ''State


obstruct :: Position -> [[Track.Cell]] -> State -> State
obstruct _ _ state@(State 0 _) = state
obstruct (Position nextPosition) rows' state
    | isObstacle $ rows' !! rowIndex !! columnIndex = state & hitPoints -~ 1
    | otherwise
    = state & position .~ Position nextPosition
  where
    columnIndex = fromIntegral $ snd nextPosition
    rowIndex = fromIntegral $ fst nextPosition

revive :: Reader Options State
revive = do
    initialPosition <- spawn
    hitPoints' <- asks _characterHitPoints
    return $ State hitPoints' initialPosition

isObstacle :: Track.Cell -> Bool
isObstacle Track.Enemy = True
isObstacle Track.Obstacle = True
isObstacle _ = False
