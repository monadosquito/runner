module Core.Port.Parser where


import Core.Configuration.Configuration

import Data.ByteString.Lazy
import Data.Proxy

import Core.Core


class Parser p where
    deserialiseConfiguration :: Proxy p -> ByteString -> Configuration
    serialiseCoreState :: Proxy p -> CoreState -> ByteString
    deserialiseCoreState :: Proxy p -> ByteString -> Maybe CoreState
    deserialisePreferences :: Proxy p -> ByteString -> Maybe Preferences
