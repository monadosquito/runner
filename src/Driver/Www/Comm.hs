{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}


module Driver.Www.Comm where


import Core.Core
import Core.Flow.Flow

import Control.Lens
import Data.Set


data MisoAct = DoNothing
             | HandleKs (Set Int)
             | Initialise
             | Pause
             | TogglePauseMode

data Act = Act Action | MisoAct MisoAct

data Mdl = Mdl { _core :: CoreState
               , _flow :: FlowState
               } deriving Eq


prefsStorItemName :: String
prefsStorItemName = "prefs"


makeFieldsNoPrefix ''Mdl
