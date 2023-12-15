{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}


module Core.Character.Character where


import Core.Configuration.Configuration
import Core.Track.Track

import Control.Lens
import Control.Monad.Reader
import Numeric.Natural


type Health = Natural


newtype Position = Position {_unPosition :: (RowIndex, ColumnIndex)} deriving Eq


data Side = Left' | Right' deriving Eq
instance Enum Side where
    fromEnum Left' = -1
    fromEnum Right' = 1

    toEnum n
        | n <= -1 = Left'
        | otherwise = Right'


makeFieldsNoPrefix ''Position


backtrack :: Position -> Position
backtrack (Position (_, columnIndex)) = Position (0, columnIndex)

progress :: Position -> Position
progress (Position (rowIndex, columnIndex))
    =
    Position (rowIndex + 1, columnIndex)

strafe :: Monad m => Side -> Position -> ReaderT Configuration m Position
strafe side (Position (rowIndex, columnIndex)) = do
    Boundaries shiftBoundaries <- getShiftBoundaries columnIndex
    return $ Position (case side of
                           Left' -> (rowIndex, fst shiftBoundaries)
                           Right' -> (rowIndex, snd shiftBoundaries)
                      )

spawn :: Monad m => ReaderT Configuration m Position
spawn = do
    trackWidth' <- asks (^. options . trackWidth)
    return $ Position (0, trackWidth' `div` 2)
