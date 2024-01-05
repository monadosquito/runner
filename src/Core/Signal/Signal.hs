module Core.Signal.Signal where


data PlayerSignal = StrafeLeft
                  | StrafeRight
                  | SwingLeft
                  | SwingRight
                  | Pause
                  | Quit
                  | Confirm
                  deriving (Enum, Eq, Read, Bounded, Ord)
instance Show PlayerSignal where
    show StrafeLeft = "Strafe Left"
    show StrafeRight = "Strafe Right"
    show SwingLeft = "Swing Left"
    show SwingRight = "Swing Right"
    show Pause = "Pause"
    show Quit = "Quit"
    show Confirm = "Confirm"

data FlowSignal = Progress

data Signal = PlayerSignal PlayerSignal | FlowSignal FlowSignal
