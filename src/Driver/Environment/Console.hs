{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Driver.Environment.Console where


import Core.Configuration.Configuration
import Core.Port.Environment

import Control.Exception
import qualified Data.ByteString.Lazy as ByteString

import Options.Applicative

import Core.Script.Track

import Core.Port.Parser

import Core.Core
import Core.Track.Configuration.Configuration
import Core.Track.Track

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Map
import System.Directory
import System.Random


data Console
instance Environment Console IO where
    getPreferences _ parser = do
        let readPrefs prefs' = Preferences
                             <$> strOption (long "track-name"
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
    getCoreState _ parser = do
        conf <- ask
        currTrackName <- asks (^. preferences . trackName)
        gen <- liftIO newStdGen
        let currTrack = tracks' ! currTrackName
            interpret'' = configure conf
            initTrackState = interpret'' currTrack gen & rows %~ reverse
        initCoreState <- initialiseCoreState initTrackState
        liftIO $ do
            savExists <- doesFileExist savFileName
            if savExists
            then do
                sav <- ByteString.readFile savFileName
                case deserialiseCoreState parser sav of
                    Just savCoreState -> return savCoreState
                    Nothing -> return initCoreState
            else do
                return initCoreState

savFileName :: String
savFileName = ".curr-rac-prog.sav"
