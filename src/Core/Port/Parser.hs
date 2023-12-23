module Core.Port.Parser where


import Core.Configuration.Configuration

import Data.ByteString.Lazy
import Data.Proxy

import Core.State


class Parser p where
    deserialiseConfiguration :: Proxy p -> ByteString -> Configuration
    serialiseExternalState :: Proxy p -> State -> ByteString
    deserialiseExternalState :: Proxy p -> ByteString -> Maybe State
    deserialisePreferences :: Proxy p -> ByteString -> Preferences
