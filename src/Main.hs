{-# LANGUAGE TypeApplications #-}


import Core.Port.Renderer

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

import Core.Track.Configuration.Configuration


main :: IO ()
main = do
    gen <- newStdGen
    conf <- getConfiguration (Proxy @Sys) . parseConfiguration $ Proxy @Aeson
    run gen conf . render $ Proxy @Cnsl
  where
    run gen conf flow = do
        let track = tracks' ! _trackName (_preferences conf)
            trackCells = List.reverse $ interpret gen track
            interpret = configure $ _options conf
        flow trackCells
