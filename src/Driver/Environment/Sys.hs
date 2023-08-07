{-# LANGUAGE TypeApplications #-}


module Driver.Environment.Sys where


import Core.Configuration.Configuration
import Core.Port.Environment

import Control.Exception
import qualified Data.ByteString.Lazy as ByteString

import Options.Applicative


data Sys
instance Environment Sys where
    getTrackName _ parseTrackName = do
        let readConfFilePath = strOption
                             $ long "configuration"
                             <> metavar "FILE_PATH"
                             <> short 'c'
                             <> value (_configurationFilePath defaultPreferences)
        confFilePath <- execParser
                     $ info (readConfFilePath <**> helper) fullDesc
        catch @IOException (parseTrackName <$> ByteString.readFile confFilePath)
            . const
            . pure
            $ _trackName defaultPreferences
