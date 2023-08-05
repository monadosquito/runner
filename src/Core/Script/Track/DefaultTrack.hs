module Core.Script.Track.DefaultTrack where


import Core.Track

import Core.Script.Track.Track1
import Core.Script.Track.Track2


defaultTrack :: Track
defaultTrack = track1 *> track2
