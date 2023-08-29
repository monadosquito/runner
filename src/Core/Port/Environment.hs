module Core.Port.Environment where


import Core.Configuration.Configuration
import Core.Signal.Signal

import Data.Proxy
import Core.Port.Parser


class Environment e where
    getConfiguration :: Parser p => Proxy e -> Proxy p -> IO Configuration
    getSignals :: Proxy e -> IO (Maybe Signal)
