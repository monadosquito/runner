module Core.Signal.Signal where


data PlayerSignal = StrafeLeft
                  | StrafeRight
                  | SwingLeft
                  | SwingRight
                  | Pause
                  | Quit
                  deriving (Enum, Eq, Read, Show, Bounded, Ord)

data FlowSignal = Progress

data Signal = PlayerSignal PlayerSignal | FlowSignal FlowSignal
