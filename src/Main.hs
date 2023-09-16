{-# LANGUAGE TypeApplications #-}


import Core.Port.Driver
import Core.Port.Environment

import Driver.Driver.Brick
import Driver.Environment.Sys
import Driver.Parser.Aeson

import Control.Monad.Reader
import Data.Proxy


main :: IO ()
main = do
    conf <- getConfiguration (Proxy @Sys) $ Proxy @Aeson
    runReaderT (run (Proxy @Brick) $ Proxy @Aeson) conf
