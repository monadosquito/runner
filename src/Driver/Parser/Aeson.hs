{-# lANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Driver.Parser.Aeson where


import Core.Configuration.Configuration
import Core.Port.Parser

import Control.Lens
import Data.Aeson.Lens
import Data.Text
import qualified Control.Monad.State as State

import Core.Character.Character
import Core.Track.Character.Character
import Core.Track.Track

import qualified Data.Aeson as Aeson
import qualified GHC.Generics as Gen

import Core.Core
import Core.Signal.Signal


data Aeson
instance Parser Aeson where
    deserialiseConfiguration _ conf
        =
        let dfltNotSetOpts = dfltNotSetOpt "trackName"
                                           (preferences . trackName)
                                           (_String . to unpack)
                           *> dfltNotSetOpt "trackWidth"
                                            (options . trackWidth)
                                            _Integral
                           *> dfltNotSetOpt "trackPieceCapacity"
                                            (preferences . trackPieceCapacity)
                                            _Integral
                           *> dfltNotSetOpt "trackDifficultyLevel"
                                            (options . trackDifficultyLevel)
                                            _Integral
                           *> dfltNotSetOpt "trackStartPartLength"
                                            (options . trackStartPartLength)
                                            _Integral
                           *> dfltNotSetOpt "characterProgressSpeed"
                                            (options . characterProgressSpeed)
                                            _Double
                           *> dfltNotSetOpt "characterHitPoints"
                                            (options . characterHitPoints)
                                            _Integral
                           *> dfltNotSetOpt "rowCrossingScoreBonus"
                                            (options . rowCrossingScoreBonus)
                                            _Integral
                           *> dfltNotSetOpt "enemyKillingScoreBonus"
                                            (options . enemyKillingScoreBonus)
                                            _Integral
            dfltNotSetOpt optName optSetter optType
                | Just optVal <- conf ^? key optName . optType
                = optSetter .= optVal
                | otherwise = return ()
        in State.execState dfltNotSetOpts defaultConfiguration
    serialiseCoreState _ = Aeson.encode
    deserialiseCoreState _ = Aeson.decode
    deserialisePreferences _ = Aeson.decode

instance Aeson.FromJSON CharacterState where
instance Aeson.FromJSON CoreState where
instance Aeson.FromJSON Position where
instance Aeson.FromJSON Cell where
instance Aeson.FromJSON Difficulty where
instance Aeson.FromJSON Slope where
instance Aeson.FromJSON TrackState where
instance Aeson.ToJSON CharacterState where
instance Aeson.ToJSON CoreState where
instance Aeson.ToJSON Position where
instance Aeson.ToJSON Cell where
instance Aeson.ToJSON Difficulty where
instance Aeson.ToJSON Slope where
instance Aeson.ToJSON TrackState where
deriving instance Gen.Generic CharacterState
deriving instance Gen.Generic CoreState
deriving instance Gen.Generic Position
deriving instance Gen.Generic Cell
deriving instance Gen.Generic Difficulty
deriving instance Gen.Generic Slope
deriving instance Gen.Generic TrackState
instance Aeson.FromJSON Preferences where
deriving instance Gen.Generic Preferences
instance Aeson.FromJSON PlayerSignal where
deriving instance Gen.Generic PlayerSignal
instance Aeson.FromJSONKey PlayerSignal where
instance Aeson.ToJSON Preferences where
instance Aeson.ToJSON PlayerSignal where
instance Aeson.ToJSONKey PlayerSignal where
instance Aeson.FromJSON Superpower where
instance Aeson.ToJSON Superpower where
deriving instance Gen.Generic Superpower
