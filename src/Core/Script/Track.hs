module Core.Script.Track where


import Core.Configuration
import Core.Track

import Core.Script.Track.DefaultTrack
import Core.Script.Track.Track1
import Core.Script.Track.Track2

import qualified Data.Map as Map


tracks :: [(TrackName, Track)]
tracks = [("track1", track1), ("track2", track2)]

tracks' :: Map.Map TrackName Track
tracks' = Map.fromList $ (_trackName defaultPreferences, defaultTrack) : tracks
