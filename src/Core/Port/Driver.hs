module Core.Port.Driver where


import Core.Configuration.Configuration

import Control.Monad.Reader
import Data.Proxy

import Core.Port.Parser


class Driver d where
    run :: Parser p => Proxy d -> Proxy p -> ReaderT Configuration IO ()
