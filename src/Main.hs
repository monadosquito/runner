{-# LANGUAGE TypeApplications #-}


import Core.Port.Driver
import Core.Port.Environment

import qualified Driver.Driver.Brick as Driver
import qualified Driver.Environment.Console as Env
import Driver.Parser.Aeson

import Control.Monad.Reader
import Data.Proxy

import Core.Configuration.Configuration


main :: IO ()
main = do
    prefs <- getPreferences (Proxy @Env.Console) $ Proxy @Aeson
    let conf = Configuration prefs defaultOptions 
    runReaderT (run (Proxy @Driver.Console) $ Proxy @Aeson) conf
