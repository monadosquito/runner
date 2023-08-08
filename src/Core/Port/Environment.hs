module Core.Port.Environment where


import Core.Configuration.Configuration

import Data.ByteString.Lazy
import Data.Proxy


class Environment e where
    getPreferences :: Proxy e -> (ByteString -> TrackName) -> IO Preferences
