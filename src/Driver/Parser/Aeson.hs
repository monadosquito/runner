{-# lANGUAGE OverloadedStrings #-}


module Driver.Parser.Aeson where


import Core.Configuration.Configuration
import Core.Port.Parser

import Control.Lens
import Data.Aeson.Lens
import Data.Text


data Aeson
instance Parser Aeson where
    parseTrackName _ conf
        | Just trackName' <- conf ^? key "trackName" . _String . to unpack
        = trackName'
        | otherwise = _trackName defaultPreferences
