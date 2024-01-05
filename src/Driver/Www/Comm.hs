{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}


module Driver.Www.Comm where


import Core.Core
import Core.Flow.Flow

import Control.Lens
import Data.Set

import Core.Signal.Signal

import Data.Map

import Core.Configuration.Configuration

import Data.Proxy
import Miso
import Miso.String
import Servant.API
import Servant.Links


type Api = MainMenu :<|> KBinds :<|> Prefs :<|> Rac
type MainMenu = View MisoAct
type KBinds = "key-bindings" :> View MisoAct
type Prefs = "preferences" :> View MisoAct
type Rac = "racing" :> View MisoAct


data MisoAct = DoNothing
             | HandleKs (Set Int)
             | Initialise
             | Pause'
             | TogglePauseMode
             | SetKBinds (Map PlayerSignal Int)
             | AlterTrackPieceCap MisoString
             | ChangeUri URI
             | HandleUri URI
             | RestDefKBinds
             | RestDefPrefs
             | SelectTrack MisoString
             | WaitNewK PlayerSignal

data Act = Act Action | MisoAct MisoAct

data Mdl = Mdl { _core :: CoreState
               , _flow :: FlowState
               , _kBinds :: Map PlayerSignal Int
               , _trackBodyPassed :: Bool
               , _boundPlayerSig :: Maybe PlayerSignal
               , _prefs :: Preferences
               , _selectedPageUri
               , _uri :: URI
               } deriving Eq


makeFieldsNoPrefix ''Mdl


prefsStorItemName :: String
prefsStorItemName = "prefs"

currRacStorItemName :: String
currRacStorItemName = "curr-rac"

mkUri :: (IsElem e a, HasLink e, MkLink e Link ~ Link)
      => Proxy a -> Proxy e -> URI
mkUri api endpoint = linkURI $ safeLink api endpoint

goTo :: (IsElem e a, HasLink e, MkLink e Link ~ Link)
     => Proxy a -> Proxy e -> Act
goTo api endpoint = MisoAct (ChangeUri $ mkUri api endpoint)
goToHome, goToKBinds, goToPrefs, goToRac :: Act
(goToHome, goToKBinds, goToPrefs, goToRac)
    =
    ( goTo (Proxy @Api) $ Proxy @MainMenu
    , goTo (Proxy @Api) $ Proxy @KBinds
    , goTo (Proxy @Api) $ Proxy @Prefs
    , goTo (Proxy @Api) $ Proxy @Rac
    )
