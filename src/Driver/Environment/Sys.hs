{-# LANGUAGE TypeApplications #-}


module Driver.Environment.Sys where


import Core.Configuration.Configuration
import Core.Port.Environment

import Control.Exception
import qualified Data.ByteString.Lazy as ByteString


data Sys
instance Environment Sys where
    getTrackName _ parseTrackName
        =
        catch @IOException
              (parseTrackName <$> ByteString.readFile "configuration.json")
        . const
        . pure
        $ _trackName defaultPreferences
