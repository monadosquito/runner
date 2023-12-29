{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Driver.Environment.Www where


import Core.Configuration.Configuration
import Core.Port.Environment
import Core.Port.Parser

import Driver.Www.Comm

import GHCJS.DOM
import GHCJS.DOM.Storage
import GHCJS.DOM.Types
import GHCJS.DOM.Window
import qualified Data.ByteString.Lazy.Char8 as BS

import Core.Core
import Core.Track.Track
import Core.Track.Configuration.Configuration

import Core.Script.Track

import Control.Lens
import Control.Monad.Reader
import Data.Map
import System.Random

import Driver.Www.Comm


#ifdef __GHCJS__
type IO' = IO
#else
type IO' = JSM
#endif


data Www
instance Environment Www IO' where
    getPreferences _ parser = do
        win <- currentWindowUnchecked
        stor <- getLocalStorage win
        prefs <- maybe defaultPreferences
                       (deserialisePreferences parser . BS.pack)
              <$> getItem @JSM @String stor prefsStorItemName
        return prefs
    getCoreState _ parser = do
        conf <- ask
        currTrackName <- asks (^. preferences . trackName)
        gen <- liftIO newStdGen
        let currTrack = tracks' ! currTrackName
            interpret'' = configure conf
            currTrackState = interpret'' currTrack gen & rows %~ reverse
        initCoreState <- initialiseCoreState currTrackState
        lift $ do
            win <- currentWindowUnchecked
            stor <- getLocalStorage win
            coreState <- maybe initCoreState
                               (maybe initCoreState id
                                . deserialiseCoreState parser . BS.pack
                               )
                      <$> getItem @JSM @String stor currRacStorItemName
            return coreState
