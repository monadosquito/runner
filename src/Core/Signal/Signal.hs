module Core.Signal.Signal where


import Core.Character.Character


data Signal = StrafeLeft | StrafeRight
            deriving (Enum, Eq, Read, Show, Bounded, Ord)

signalToSide :: Signal -> Side
signalToSide StrafeLeft = Left'
signalToSide StrafeRight = Right'
