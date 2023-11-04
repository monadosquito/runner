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
import qualified Core.Track.Character.Character as Character
import qualified Core.Track.Track as Track

import qualified Data.Aeson as Aeson
import qualified GHC.Generics as Gen

import Core.State


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
    serialiseExternalState _ = Aeson.encode
    deserialiseExternalState _ = Aeson.decode

instance Aeson.FromJSON Character.State where
instance Aeson.FromJSON State where
instance Aeson.FromJSON Position where
instance Aeson.FromJSON Track.Cell where
instance Aeson.FromJSON Track.Difficulty where
instance Aeson.FromJSON Track.Slope where
instance Aeson.FromJSON Track.State where
instance Aeson.ToJSON Character.State where
instance Aeson.ToJSON State where
instance Aeson.ToJSON Position where
instance Aeson.ToJSON Track.Cell where
instance Aeson.ToJSON Track.Difficulty where
instance Aeson.ToJSON Track.Slope where
instance Aeson.ToJSON Track.State where
deriving instance Gen.Generic Character.State
deriving instance Gen.Generic State
deriving instance Gen.Generic Position
deriving instance Gen.Generic Track.Cell
deriving instance Gen.Generic Track.Difficulty
deriving instance Gen.Generic Track.Slope
deriving instance Gen.Generic Track.State
