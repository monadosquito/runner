module Core.Script.Track.DefaultTrack where


import Core.Track

import Core.Script.Track.Track1
import Core.Script.Track.Track2


defaultTrack :: Track
defaultTrack = do
    track1
    withDifficultyLevel 0
    track2
