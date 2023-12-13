module Core.Track.Configuration.Configuration ( module Core.Track.Configuration.Configuration
                                              , module Core.Configuration.Configuration
                                              ) where


import Core.Configuration.Configuration
import Core.Track.Track

import Control.Monad.Reader
import qualified Control.Monad.State as State
import System.Random

import qualified Core.Configuration.Configuration as Configuration

import Control.Lens


configure :: Configuration -> Track -> StdGen -> TrackState
configure configuration track' generator'
    =
    _track $ runReader initialise $ configuration & options %~ Configuration.fix
  where
    initialise = do
        initialState <- initialiseTrackState
        let initialGenerationState = initialiseGenerationState generator'
                                                               initialState
        State.execStateT (interpret' track') initialGenerationState

configureFrom :: Configuration -> TrackState -> Track -> StdGen -> TrackState
configureFrom configuration
    =
    \initialState track' generator'
    ->
    let initialise = State.execStateT (interpret' track') initialGenerationState
        initialGenerationState = initialiseGenerationState generator'
                                                           initialState
    in
        _track . runReader initialise
               $ configuration & options %~ Configuration.fix
