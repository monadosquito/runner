module Core.Track ( Track.Track
                  , Track.staticLengthFinitePart
                  , Track.withDifficultyLevel
                  , Track.withAlteredDifficultyLevel
                  , Track.withDifficultyLevelAmount
                  , Track.withAmountAlteredDifficultyLevel
                  , Track.sequenceEnd
                  , Track.eitherSequenceWhere
                  , Track.withProbability
                  , Track.withGradualDifficultyLevelSlope
                  , Track.withSteepDifficultyLevelSlope
                  , Track.withGradualDifficultyLevelAmountRiseSlope
                  , Track.infiniteTailWhere
                  , Track.middlePredefinedPart
                  , Track.leftPredefinedPart
                  , Track.rightPredefinedPart
                  , Track.Cell (..)
                  , Track.repeatedSequenceWhere
                  , Track.dynamicLengthFinitePart
                  ) where


import qualified Core.Track.Track as Track
