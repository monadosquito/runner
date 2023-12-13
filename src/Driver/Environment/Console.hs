{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}


module Driver.Environment.Console where


import Core.Configuration.Configuration
import Core.Port.Environment

import Control.Exception
import qualified Data.ByteString.Lazy as ByteString

import Options.Applicative

import Core.Script.Track

import Core.Port.Parser


data Console
instance Environment Console where
    getPreferences _ parser = do
        let readPrefs prefs' = Preferences
                             <$> strOption (completeWith (map fst tracks)
                                            <> long "track-name"
                                            <> metavar "STRING"
                                            <> short 't'
                                            <> value (_trackName prefs')
                                           )
                             <*> strOption (long "configuration"
                                            <> metavar "FILE_PATH"
                                            <> short 'c'
                                            <> value (_configurationFilePath defaultPreferences)
                                           )
                             <*> option auto
                                     (long "track-piece-capacity"
                                      <> metavar "NATURAL_NUMBER"
                                      <> short 'p'
                                      <> value (_trackPieceCapacity prefs')
                                     )
        argPrefs <- execParser
                 $ info (readPrefs defaultPreferences <**> helper) fullDesc
        fileConf <- catch @IOException
                          (deserialisePreferences parser
                           <$> ByteString.readFile (_configurationFilePath argPrefs)
                          )
                 . const
                 $ pure defaultPreferences
        execParser $ info (readPrefs fileConf <**> helper) fullDesc
