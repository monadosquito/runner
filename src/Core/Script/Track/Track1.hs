module Core.Script.Track.Track1 where


import Core.Track


track1 :: Track
track1 = middlePredefinedPart Obstacle
                              [ [TrailPart, TrailPart, TrailPart]
                              , [TrailPart, TrailPart, TrailPart]
                              ]
       *> leftPredefinedPart Obstacle
                             [ [TrailPart, TrailPart, TrailPart]
                             , [TrailPart, TrailPart, TrailPart]
                             ]
       *> rightPredefinedPart Obstacle
                              [ [TrailPart, TrailPart, TrailPart]
                              , [TrailPart, TrailPart, TrailPart]
                              ]
       *> eitherSequenceWhere
       *> withProbability 0.9
       *> finitePart 10
       *> eitherSequenceWhere
       *> withAmountAlteredDifficultyLevel 0.5
       *> withProbability 0.1
       *> finitePart 10
       *> eitherSequenceEnd
       *> withGradualDifficultyLevelAmountRiseSlope 0.1 2
       *> finitePart 10
       *> withSteepDifficultyLevelSlope
       *> finitePart 10
       *> infiniteTailWhere
       *> finitePart 10
