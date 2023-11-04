module Core.Signal.Signal where


import Core.Character.Character


data PlayerSignal = StrafeLeft | StrafeRight
                  deriving (Enum, Eq, Read, Show, Bounded, Ord)

data FlowSignal = Progress

data Signal = PlayerSignal PlayerSignal | FlowSignal FlowSignal


signalToSide :: PlayerSignal -> Side
signalToSide StrafeLeft = Left'
signalToSide StrafeRight = Right'
