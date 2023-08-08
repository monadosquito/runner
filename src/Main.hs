{-# LANGUAGE TypeApplications #-}


import Core.Port.Renderer
import Core.Track.Track

import Driver.Renderer.Cnsl

import Data.Proxy
import System.Random
import qualified Data.List.NonEmpty as List

import Core.Port.Environment
import Core.Port.Parser

import Core.Script.Track

import Driver.Environment.Sys
import Driver.Parser.Aeson

import Data.Map.NonEmpty

import Core.Configuration.Configuration


main :: IO ()
main = do
    gen <- newStdGen
    prefs <- getPreferences (Proxy @Sys) . parseTrackName $ Proxy @Aeson
    run gen prefs . render $ Proxy @Cnsl
  where
    run gen prefs flow = do
        let track = tracks' ! _trackName prefs
        flow . List.reverse $ interpret gen track
