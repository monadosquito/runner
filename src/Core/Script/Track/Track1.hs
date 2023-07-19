module Core.Script.Track.Track1 where


import Core.Track


track1 :: Track
track1 = eitherSequenceWhere
       *> part 10
       *> eitherSequenceWhere
       *> withAmountAlteredDifficultyLevel 0.5
       *> part 10
       *> eitherSequenceEnd
       *> withAmountAlteredDifficultyLevel 1
       *> part 10
