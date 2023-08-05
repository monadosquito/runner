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


main :: IO ()
main = do
    gen <- newStdGen
    trackName <- getTrackName (Proxy @Sys) . parseTrackName $ Proxy @Aeson
    run gen trackName . render $ Proxy @Cnsl
  where
    run gen trackName flow = do
        let track = tracks' ! trackName
        flow . List.reverse $ interpret gen track
