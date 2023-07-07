{-# LANGUAGE TypeApplications #-}


module Driver.Environment.Sys where


import Core.Configuration.Configuration
import Core.Port.Environment

import Control.Exception
import qualified Data.ByteString.Lazy as ByteString

import Options.Applicative
import Control.Lens

import Core.Script.Track


data Sys
instance Environment Sys where
    getConfiguration _ parseConf = do
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
            readConf (Configuration prefs' opts) = Configuration
                                                 <$> readPrefs prefs'
                                                 <*> readOpts opts
            readOpts opts = Options <$> option auto (long "track-width"
                                                     <> metavar "NATURAL_NUMBER"
                                                     <> short 'w'
                                                     <> value (_trackWidth opts)
                                                    )
                                    <*> option auto
                                               (long "track-difficulty-level"
                                                <> metavar "NATURAL_NUMBER"
                                                <> short 'd'
                                                <> value (_trackDifficultyLevel opts)
                                               )
        argConf <- execParser
                 $ info (readConf defaultConfiguration <**> helper)
                        fullDesc
        fileConf <- catch @IOException
                          (parseConf
                           <$> ByteString.readFile (argConf
                                                    ^. preferences
                                                    . configurationFilePath
                                                   )
                          )
                  . const
                  $ pure defaultConfiguration
        execParser $ info (readConf fileConf <**> helper) fullDesc
