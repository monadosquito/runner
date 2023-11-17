module Core.Script.Track.Track1 where


import Core.Track


track1 :: Track
track1 = do
    repeatedSequenceWhere 5
    middlePredefinedPart TrailPart [[]]
    repeatedSequenceWhere 2
    middlePredefinedPart TrailPart [obstacle 1 ++ trailPart 1 ++ obstacle 1]
    sequenceEnd
    sequenceEnd
    eitherSequenceWhere
    withProbability 0.4
    withDifficultyLevel 0
    staticLengthFinitePart 10
    eitherSequenceWhere
    withProbability 0.4
    withDifficultyLevelAmount 0.5
    dynamicLengthFinitePart (10, 20)
    eitherSequenceWhere
    withProbability 0.2
    middlePredefinedPart TrailPart [[]]
    sequenceEnd
    middlePredefinedPart TrailPart [obstacle 1 ++ trailPart 1 ++ obstacle 1]
    repeatedSequenceWhere 4
    middlePredefinedPart TrailPart [obstacle 1 ++ trailPart 1 ++ obstacle 1]
    sequenceEnd
    middlePredefinedPart TrailPart [obstacle 1 ++ trailPart 1 ++ obstacle 1]
  where
    obstacle = (`replicate` Obstacle)
    trailPart = (`replicate` TrailPart)
