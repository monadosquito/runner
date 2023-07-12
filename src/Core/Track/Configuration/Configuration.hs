module Core.Track.Configuration.Configuration ( module Core.Track.Configuration.Configuration
                                              , module Core.Configuration.Configuration
                                              ) where


import Core.Configuration.Configuration
import Core.Track.Track

import Control.Monad.Reader
import qualified Control.Monad.State as State
import System.Random

import qualified Core.Configuration.Configuration as Configuration


configure :: Configuration.Options -> StdGen -> Track -> GenerationState
configure options' generator' track = runReader initialise
                                    $ Configuration.fix options'
  where
    initialise = do
         generationState <- initialGenerationState generator'
         State.execStateT (interpret' track) generationState

configureFrom :: Options -> GenerationState -> Track -> GenerationState
configureFrom options' generationState track = runReader initialise
                                             $ Configuration.fix options'
  where
    initialise = State.execStateT (interpret' track) generationState
