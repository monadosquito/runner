{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}


module Driver.Www.Comm where


import Core.Core
import Core.Flow.Flow

import Control.Lens
import Data.Set

import Core.Signal.Signal

import Data.Map


data MisoAct = DoNothing
             | HandleKs (Set Int)
             | Initialise
             | Pause'
             | TogglePauseMode
             | SetKBinds (Map PlayerSignal Int)

data Act = Act Action | MisoAct MisoAct

data Mdl = Mdl { _core :: CoreState
               , _flow :: FlowState
               , _kBinds :: Map PlayerSignal Int
               , _trackBodyPassed :: Bool
               } deriving Eq

prefsStorItemName :: String
prefsStorItemName = "prefs"

currRacStorItemName :: String
currRacStorItemName = "curr-rac"


makeFieldsNoPrefix ''Mdl
