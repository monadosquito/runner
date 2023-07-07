{-# lANGUAGE OverloadedStrings #-}


module Driver.Parser.Aeson where


import Core.Configuration.Configuration
import Core.Port.Parser

import Control.Lens
import Data.Aeson.Lens
import Data.Text
import Control.Monad.State


data Aeson
instance Parser Aeson where
    parseConfiguration _ conf
        =
        let dfltNotSetOpts = dfltNotSetOpt "trackName"
                                           (preferences . trackName)
                                           (_String . to unpack)
                           *> dfltNotSetOpt "trackWidth"
                                            (options . trackWidth)
                                            _Integral
            dfltNotSetOpt optName optSetter optType
                | Just optVal <- conf ^? key optName . optType
                = optSetter .= optVal
                | otherwise = return ()
        in execState dfltNotSetOpts defaultConfiguration
