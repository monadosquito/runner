module Core.Port.Parser where


import Core.Configuration.Configuration

import Data.ByteString.Lazy
import Data.Proxy


class Parser p where
    deserialiseConfiguration :: Proxy p -> ByteString -> Configuration
