module Core.Script.Track.Track1 where


import Core.Track


track1 :: Track
track1 = eitherSequenceWhere
       *> withProbability 0.9
       *> part 10
       *> eitherSequenceWhere
       *> withAmountAlteredDifficultyLevel 0.5
       *> withProbability 0.1
       *> part 10
       *> eitherSequenceEnd
       *> withGradualDifficultyLevelAmountRiseSlope 0.1 2
       *> part 10
       *> withAmountAlteredDifficultyLevel 0.5
       *> withSteepDifficultyLevelSlope
       *> part 10
