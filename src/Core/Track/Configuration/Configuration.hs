module Core.Track.Configuration.Configuration ( module Core.Track.Configuration.Configuration
                                              , module Core.Configuration.Configuration
                                              ) where


import Core.Configuration.Configuration
import Core.Track.Track

import Control.Monad.Reader
import qualified Control.Monad.State as State
import System.Random

import qualified Core.Configuration.Configuration as Configuration


configure :: Options -> Track -> StdGen -> State
configure options' track' generator'
    =
    _track . runReader initialise $ Configuration.fix options'
  where
    initialise = do
        initialState <- initialiseState
        let initialGenerationState = initialiseGenerationState generator'
                                                               initialState
        State.execStateT (interpret' track') initialGenerationState

configureFrom :: Options -> State -> Track -> StdGen -> State
configureFrom options'
    =
    \state track' generator'
    ->
    let initialise = State.execStateT (interpret' track') initialGenerationState
        initialGenerationState = initialiseGenerationState generator' state
    in
        _track . runReader initialise $ Configuration.fix options'
