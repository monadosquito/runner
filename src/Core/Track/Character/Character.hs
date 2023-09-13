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


reflect :: State -> [[Track.Cell]] -> [[Track.Cell]]
reflect (State _ (Position position')) rows'
    =
    rows' & element rowIndex . element columnIndex .~ Track.Character
  where
    rowIndex = fromIntegral $ fst position'
    columnIndex = fromIntegral $ snd position'

obstruct :: Position -> [[Track.Cell]] -> State -> State
obstruct _ _ state@(State 0 _) = state
obstruct (Position nextPosition) rows' state
    | rows' !! rowIndex !! columnIndex == Track.Obstacle
    = state & hitPoints -~ 1
    | otherwise = state & position .~ Position nextPosition
  where
    columnIndex = fromIntegral $ snd nextPosition
    rowIndex = fromIntegral $ fst nextPosition

revive :: Reader Options State
revive = do
    initialPosition <- spawn
    return $ State hitPoints' initialPosition
