{-# LANGUAGE TypeApplications #-}


import Core.Port.Renderer
import Core.Script.Track.Track1
import Core.Track.Track

import Driver.Renderer.Cnsl

import Data.Proxy
import System.Random
import qualified Data.List.NonEmpty as List


main :: IO ()
main = do
    gen <- newStdGen
    run gen . render $ Proxy @Cnsl
  where
    run gen flow = flow . List.reverse $ interpret gen track1
