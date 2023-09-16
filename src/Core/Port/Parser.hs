{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}


module Core.Port.Parser where


import Core.Configuration.Configuration

import Data.ByteString.Lazy
import Data.Proxy

import qualified Core.Track.Character.Character as Character
import qualified Core.Track.Track as Track

import Control.Lens
import Numeric.Natural


data ExternalState = ExternalState { _character :: Character.State
                                   , _score :: Natural
                                   , _track :: Track.State
                                   }


makeFieldsNoPrefix ''ExternalState


class Parser p where
    deserialiseConfiguration :: Proxy p -> ByteString -> Configuration
    serialiseExternalState :: Proxy p -> ExternalState -> ByteString
    deserialiseExternalState :: Proxy p -> ByteString -> Maybe ExternalState
