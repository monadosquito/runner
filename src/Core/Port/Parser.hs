module Core.Port.Parser where


import Core.Configuration.Configuration

import Data.ByteString.Lazy
import Data.Proxy


class Parser p where
    parseTrackName :: Proxy p -> ByteString -> TrackName
