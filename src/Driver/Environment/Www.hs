{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Driver.Environment.Www where


import Core.Configuration.Configuration
import Core.Port.Environment
import Core.Port.Parser

import GHCJS.DOM
import GHCJS.DOM.Storage
import GHCJS.DOM.Types
import GHCJS.DOM.Window
import qualified Data.ByteString.Lazy.Char8 as BS

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
              <$> getItem @JSM @String stor ("prefs" :: String)
        return prefs
