module Core.Script.Track.Track2 where


import Core.Track

import Core.Script.Track.Track1


track2 :: Track
track2 = track1 *> track1
