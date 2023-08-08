{-# LANGUAGE TypeApplications #-}


module Driver.Environment.Sys where


import Core.Configuration.Configuration
import Core.Port.Environment

import Control.Exception
import qualified Data.ByteString.Lazy as ByteString

import Options.Applicative

import Core.Script.Track


data Sys
instance Environment Sys where
    getPreferences _ parsePrefs = do
        let readPrefs trackName = Preferences
                                <$> strOption (completeWith (map fst tracks)
                                               <> long "track-name"
                                               <> metavar "STRING"
                                               <> short 't'
                                               <> value trackName
                                              )
                                <*> strOption (long "configuration"
                                               <> metavar "FILE_PATH"
                                               <> short 'c'
                                               <> value (_configurationFilePath defaultPreferences)
                                              )
        argPrefs <- execParser
                 $ info (readPrefs (_trackName defaultPreferences) <**> helper)
                        fullDesc
        filePrefs <- catch @IOException
                          (parsePrefs
                           <$> ByteString.readFile (_configurationFilePath argPrefs)
                          )
                  . const
                  . pure
                  $ _trackName defaultPreferences
        execParser $ info (readPrefs filePrefs <**> helper) fullDesc
