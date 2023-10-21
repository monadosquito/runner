module Core.Script.Track.Track2 where


import Core.Track


track2 :: Track
track2 = do
    infiniteTailWhere
    withGradualDifficultyLevelAmountRiseSlope 0.1 2
    staticLengthFinitePart 10
