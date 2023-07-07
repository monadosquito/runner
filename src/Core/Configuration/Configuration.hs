{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}


module Core.Configuration.Configuration where


import Control.Lens
import Numeric.Natural


type TrackName = String

type TrackWidth = Natural


data Configuration = Configuration { _preferences :: Preferences
                                   , _options :: Options
                                   }

data Preferences = Preferences { _trackName :: TrackName
                               , _configurationFilePath :: FilePath
                               }

data Options = Options { _trackWidth :: TrackWidth
                       }


makeFieldsNoPrefix ''Configuration
makeFieldsNoPrefix ''Options
makeFieldsNoPrefix ''Preferences


defaultPreferences :: Preferences
defaultPreferences = Preferences { _trackName = "default"
                                 , _configurationFilePath = "configuration.json"
                                 }

defaultConfiguration :: Configuration
defaultConfiguration = Configuration { _preferences = defaultPreferences
                                     , _options = defaultOptions
                                     }

defaultOptions :: Options
defaultOptions = Options { _trackWidth = 5
                         }
