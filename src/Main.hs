{-# LANGUAGE TypeApplications #-}


import Core.Port.Driver

import qualified Driver.Driver.Www.Www as Driver
import Driver.Parser.Aeson

import Control.Monad.Reader
import Data.Proxy

import Core.Configuration.Configuration
import Core.Port.Environment
import qualified Driver.Environment.Www as Env


main :: IO ()
main = do
    prefs <- getPreferences (Proxy @Env.Www) $ Proxy @Aeson
    let conf = Configuration prefs defaultOptions 
    runReaderT (run (Proxy @Driver.Www) $ Proxy @Aeson) conf
