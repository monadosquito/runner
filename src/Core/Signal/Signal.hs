module Core.Signal.Signal where


import Core.Character.Character


data Signal = StrafeLeft | StrafeRight deriving (Enum, Eq, Read, Show)

signalToSide :: Signal -> Side
signalToSide StrafeLeft = Left'
signalToSide StrafeRight = Right'
