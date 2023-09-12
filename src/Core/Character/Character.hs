{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}


module Core.Character.Character where


import Core.Configuration.Configuration
import Core.Track.Track

import Control.Lens
import Control.Monad.Reader


newtype Position = Position (RowIndex, ColumnIndex)


data Side = Left' | Right' deriving Eq


makeFieldsNoPrefix ''Position


backtrack :: Position -> Position
backtrack (Position (_, columnIndex)) = Position (0, columnIndex)

progress :: Position -> Position
progress (Position (rowIndex, columnIndex))
    =
    Position (rowIndex + 1, columnIndex)

strafe :: Side -> Position -> Reader Options Position
strafe side (Position (rowIndex, columnIndex)) = do
    Boundaries shiftBoundaries <- getShiftBoundaries columnIndex
    return $ Position (case side of
                           Left' -> (rowIndex, fst shiftBoundaries)
                           Right' -> (rowIndex, snd shiftBoundaries)
                      )

spawn :: Reader Options Position
spawn = do
    trackWidth' <- asks (^. trackWidth)
    return $ Position (0, trackWidth' `div` 2)
