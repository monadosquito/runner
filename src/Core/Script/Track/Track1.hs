module Core.Script.Track.Track1 where


import Core.Track


track1 :: Track
track1 = repeatedSequenceWhere 2
       *> middlePredefinedPart Obstacle [[TrailPart, TrailPart, TrailPart]]
       *> sequenceEnd
       *> repeatedSequenceWhere 2
       *> leftPredefinedPart Obstacle [[TrailPart, TrailPart, TrailPart]]
       *> sequenceEnd
       *> repeatedSequenceWhere 2
       *> rightPredefinedPart Obstacle [[TrailPart, TrailPart, TrailPart]]
       *> sequenceEnd
       *> eitherSequenceWhere
       *> withProbability 0.9
       *> dynamicLengthFinitePart (5, 15)
       *> eitherSequenceWhere
       *> withAmountAlteredDifficultyLevel 0.5
       *> withProbability 0.1
       *> dynamicLengthFinitePart (5, 10)
       *> sequenceEnd
       *> withGradualDifficultyLevelAmountRiseSlope 0.1 2
       *> staticLengthFinitePart 10
       *> withSteepDifficultyLevelSlope
       *> staticLengthFinitePart 10
       *> infiniteTailWhere
       *> staticLengthFinitePart 10
