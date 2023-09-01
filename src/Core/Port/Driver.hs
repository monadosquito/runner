module Core.Port.Driver where


import Core.Configuration.Configuration

import Control.Monad.Reader
import Data.Proxy


class Driver d where
    run :: Proxy d -> ReaderT Configuration IO ()
