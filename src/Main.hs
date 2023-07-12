{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}


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

import qualified Data.Map.NonEmpty as Map

import Core.Configuration.Configuration

import Core.Track.Configuration.Configuration

import Control.Concurrent
import Data.Foldable

import Core.Track.Track


data FlowInput = FlowInput { trackCells :: List.NonEmpty [Cell]
                           , trackPiecesCnt :: Int
                           , trackRem :: Maybe (List.NonEmpty [Cell])
                           }


main :: IO ()
main = do
    gen <- newStdGen
    conf <- getConfiguration (Proxy @Sys) . parseConfiguration $ Proxy @Aeson
    run gen conf $ \FlowInput {..} -> do
        forM_ [0..trackPiecesCnt - 1] $ \ix -> do
            rndrTrackPiece ix trackCells
            threadDelay 1000000
        rndr trackRem
  where
    run gen conf flow = do
        let track = tracks' Map.! _trackName (_preferences conf)
            trackCells = List.reverse $ interpret'' gen track
            interpret'' = configure $ _options conf
            trackPiecesCnt = List.length trackCells `div` trackPieceCap
            trackRem = List.nonEmpty
                     $ List.drop (trackPieceCap * trackPiecesCnt) trackCells
        flow FlowInput {..}
    rndr (Just trackPiece) = render (Proxy @Cnsl) trackPiece
    rndr Nothing = pure ()
    rndrTrackPiece ix' cells' = do
        let trackPiece = take trackPieceCap
                       $ List.drop (ix' * trackPieceCap) cells'
        rndr $ List.nonEmpty trackPiece

trackPieceCap :: Int
trackPieceCap = 10
