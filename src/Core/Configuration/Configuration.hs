{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}


module Core.Configuration.Configuration where


import Control.Lens
import Numeric.Natural


type TrackName = String

type TrackWidth = Natural

type TrackPieceCapacity = Natural

type TrackDifficultyLevel = Natural


data Configuration = Configuration { _preferences :: Preferences
                                   , _options :: Options
                                   }

data Preferences = Preferences { _trackName :: TrackName
                               , _configurationFilePath :: FilePath
                               , _trackPieceCapacity :: TrackPieceCapacity
                               }

data Options = Options { _trackWidth :: TrackWidth
                       , _trackDifficultyLevel :: TrackDifficultyLevel
                       , _trackStartPartLength :: Natural
                       , _characterProgressSpeed :: Double
                       , _characterHitPoints :: Natural
                       }


makeFieldsNoPrefix ''Configuration
makeFieldsNoPrefix ''Options
makeFieldsNoPrefix ''Preferences


defaultPreferences :: Preferences
defaultPreferences = Preferences { _trackName = "default"
                                 , _configurationFilePath = "conf.json"
                                 , _trackPieceCapacity = 10
                                 }

defaultConfiguration :: Configuration
defaultConfiguration = Configuration { _preferences = defaultPreferences
                                     , _options = defaultOptions
                                     }

defaultOptions :: Options
defaultOptions = Options { _trackWidth = 5
                         , _trackDifficultyLevel = _trackWidth defaultOptions
                                                 `div` 2
                         , _trackStartPartLength = 3
                         , _characterProgressSpeed = 2
                         , _characterHitPoints = 3
                         }

fix :: Options -> Options
fix options'@Options {..}
    =
    options' { _trackDifficultyLevel = trackDifficultyLevel'
             }
  where
    trackDifficultyLevel' = if _trackDifficultyLevel > _trackWidth
                            then defaultOptions ^. trackDifficultyLevel
                            else _trackDifficultyLevel
