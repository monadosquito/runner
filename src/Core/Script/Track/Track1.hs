module Core.Script.Track.Track1 where


import Core.Track


track1 :: Track
track1 = part 10 *> withDifficultyLevelAmount 1 *> part 10
