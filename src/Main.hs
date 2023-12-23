{-# LANGUAGE TypeApplications #-}


import Core.Port.Driver
import Core.Port.Environment

import Driver.Driver.Brick
import Driver.Environment.Sys
import Driver.Parser.Aeson

import Control.Monad.Reader
import Data.Proxy

import Core.Configuration.Configuration


main :: IO ()
main = do
    prefs <- getPreferences (Proxy @Sys) $ Proxy @Aeson
    let conf = Configuration prefs defaultOptions 
    runReaderT (run (Proxy @Brick) $ Proxy @Aeson) conf
