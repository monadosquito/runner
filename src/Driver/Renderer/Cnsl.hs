module Driver.Renderer.Cnsl ( Cnsl
                            ) where


import Core.Port.Renderer
import Core.Track.Track

import Data.List.NonEmpty


data Cnsl
instance Renderer Cnsl where
    render _ = print . RndredTrackLines

newtype RndredTrackCell = RndredTrackCell Cell
instance Show RndredTrackCell where
    show (RndredTrackCell Obstacle) = "="
    show (RndredTrackCell TrailPart) = "."

newtype RndredTrackLines = RndredTrackLines (NonEmpty [Cell])
instance Show RndredTrackLines where
    show (RndredTrackLines lines')
        =
        foldr (\trackLine shownTrack
               ->
               shownTrack
               ++ "\n"
               ++ concat (show . RndredTrackCell <$> trackLine)
              )
              ""
              lines'
