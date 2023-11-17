{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Core.Port.Environment where


import Core.Configuration.Configuration

import Data.Proxy
import Core.Port.Parser


class Environment e m where
    getPreferences :: Parser p => Proxy e -> Proxy p -> m Preferences
