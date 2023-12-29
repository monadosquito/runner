{-# LANGUAGE MultiParamTypeClasses #-}


module Core.Port.Driver where


import Core.Configuration.Configuration

import Control.Monad.Reader
import Data.Proxy

import Core.Port.Parser

import Core.Core


class Driver d where
    run :: Parser p
        => Proxy d
        -> Proxy p
        -> CoreState
        -> ReaderT Configuration IO ()
