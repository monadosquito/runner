module Core.Configuration.Configuration where


type TrackName = String


data Configuration = Configuration { _preferences :: Preferences
                                   }

data Preferences = Preferences { _trackName :: TrackName
                               , _configurationFilePath :: FilePath
                               }


defaultPreferences :: Preferences
defaultPreferences = Preferences { _trackName = "default"
                                 , _configurationFilePath = "configuration.json"
                                 }

defaultConfiguration :: Configuration
defaultConfiguration = Configuration { _preferences = defaultPreferences
                                     }
