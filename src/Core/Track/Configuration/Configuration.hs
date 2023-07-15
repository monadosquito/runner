module Core.Track.Configuration.Configuration ( module Core.Track.Configuration.Configuration
                                              , module Core.Configuration.Configuration
                                              ) where


import Core.Configuration.Configuration
import Core.Track.Track

import Control.Lens
import Control.Monad.Reader
import qualified Control.Monad.State as State
import System.Random
import Data.List.NonEmpty

import qualified Core.Configuration.Configuration as Configuration


configure :: Configuration.Options -> StdGen -> Track -> NonEmpty [Cell]
configure options' generator' track = runReader initialise
                                                (Configuration.fix options')
                                    ^. cells
  where
    initialise = do
         generationState <- initialGenerationState generator'
         State.execStateT (interpret' track) generationState
