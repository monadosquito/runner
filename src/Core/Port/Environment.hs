module Core.Port.Environment where


import Core.Configuration.Configuration

import Data.ByteString.Lazy
import Data.Proxy


class Environment e where
    getTrackName :: Proxy e -> (ByteString -> TrackName) -> IO TrackName
