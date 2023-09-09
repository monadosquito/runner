module Core.Signal.Signal where


import Core.Character.Character


data Signal = StrafeCharacter Side deriving Eq
instance Enum Signal where
    fromEnum (StrafeCharacter Left') = 0
    fromEnum (StrafeCharacter Right') = 1

    toEnum 0 = StrafeCharacter Left'
    toEnum 1 = StrafeCharacter Right'
    toEnum _ = error ""
instance Show Signal where
    show (StrafeCharacter Left') = "Strafe Left"
    show (StrafeCharacter Right') = "Strafe Right"
