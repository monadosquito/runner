{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}


module Core.Track.Character.Character where


import Core.Character.Character
import Core.Track.Track

import Control.Lens

import Core.Configuration.Configuration

import Control.Monad.Reader
import Numeric.Natural


data CharacterState = CharacterState { _hitPoints :: Natural
                                     , _position :: Position
                                     , _superpower :: Maybe Superpower
                                     } deriving Eq

data Superpower = BerserkerSuperpower Natural deriving Eq


makeFieldsNoPrefix ''CharacterState
makePrisms ''Superpower


obstruct :: Position -> [[Cell]] -> CharacterState -> CharacterState
obstruct (Position nextPosition)
         _
         state@(CharacterState _ _ (Just (BerserkerSuperpower 0)))
    =
    state & superpower .~ Nothing & position .~ Position nextPosition
obstruct (Position nextPosition)
         _
         state@(CharacterState _ _ (Just (BerserkerSuperpower _)))
    =
    state & superpower . _Just . _BerserkerSuperpower -~ 1
          & position .~ Position nextPosition
obstruct (Position nextPosition) rows' state
    | isObstacle $ rows' !! rowIndex !! columnIndex = state & hitPoints -~ 1
    | otherwise = state & position .~ Position nextPosition
  where
    columnIndex = fromIntegral $ snd nextPosition
    rowIndex = fromIntegral $ fst nextPosition

revive :: Monad m => ReaderT Configuration m CharacterState
revive = do
    initialPosition <- spawn
    hitPoints' <- asks (^. options . characterHitPoints)
    return $ CharacterState hitPoints' initialPosition Nothing

isObstacle :: Cell -> Bool
isObstacle LivingEnemy = True
isObstacle Obstacle = True
isObstacle _ = False

kill :: Cell ->  Cell
kill LivingEnemy = DeadEnemy
kill cell = cell

berserkerSuperpower :: Superpower
berserkerSuperpower = BerserkerSuperpower 10
