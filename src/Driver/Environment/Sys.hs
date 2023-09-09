{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}


module Driver.Environment.Sys where


import Core.Configuration.Configuration
import Core.Port.Environment

import Control.Exception
import qualified Data.ByteString.Lazy as ByteString

import Options.Applicative
import Control.Lens

import Core.Script.Track

import Core.Port.Parser
import Core.Character.Character
import Core.Signal.Signal

import System.IO


data Sys
instance Environment Sys where
    getConfiguration _ parser = do
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
                                    <*> option auto
                                               (long "track-start-part-length"
                                                <> metavar "NATURAL_NUMBER"
                                                <> short 'l'
                                                <> value (_trackStartPartLength opts)
                                               )
        argConf <- execParser
                 $ info (readConf defaultConfiguration <**> helper)
                        fullDesc
        fileConf <- catch @IOException
                          (deserialiseConfiguration parser
                           <$> ByteString.readFile (argConf
                                                    ^. preferences
                                                    . configurationFilePath
                                                   )
                          )
                  . const
                  $ pure defaultConfiguration
        execParser $ info (readConf fileConf <**> helper) fullDesc
    getSignals _ = do
        key <- getArrK
        return $ if | key == Right 'q' -> Nothing
                    | key `elem` [Left (Just LeftArrK), Right 'a']
                    -> Just (StrafeCharacter Left')
                    | key `elem` [Left (Just RightArrK), Right 'd']
                    -> Just (StrafeCharacter Right')
                    | otherwise -> Nothing

data ArrK = LeftArrK | RightArrK | UpArrK | DownArrK deriving Eq


getArrK :: IO (Either (Maybe ArrK) Char)
getArrK = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdout False
    c <- getChar
    case c of
        '\ESC' -> Left <$> parseArr
        char -> return $ Right char

parseArr :: IO (Maybe ArrK)
parseArr = do
    char <- getChar
    case char of
        '\ESC' -> parseArr
        '[' -> do
            ltr <- getChar
            return $ case ltr of
                         'A' -> Just UpArrK
                         'B' -> Just DownArrK
                         'C' -> Just RightArrK
                         'D' -> Just LeftArrK
                         _   -> Nothing
        _ -> return Nothing
