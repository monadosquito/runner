module Driver.Renderer.Cnsl ( Cnsl
                            ) where


import Core.Port.Renderer
import Core.Track.Track


data Cnsl
instance Renderer Cnsl where
    render _ = print . RndredTrackLines

newtype RndredTrackCell = RndredTrackCell Cell
instance Show RndredTrackCell where
    show (RndredTrackCell Obstacle) = "="
    show (RndredTrackCell TrailPart) = "."
    show (RndredTrackCell Pass) = ","

newtype RndredTrackLines = RndredTrackLines [[Cell]]
instance Show RndredTrackLines where
    show (RndredTrackLines lines')
        =
        foldr (\trackLine shownTrack
               ->
               shownTrack
               ++ "\n"
               ++ concat (show . RndredTrackCell <$> trackLine)
              )
              "\ESC[2J"
              lines'
