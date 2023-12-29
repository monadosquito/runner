{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}


module Core.Flow.Flow where


import Core.Configuration.Configuration
import Core.Signal.Signal

import Control.Concurrent
import Control.Lens
import Control.Monad.Free
import Control.Monad.Reader
import System.Random


type Flow r = Free (Flow' r)


data Flow' r n = Dispatch Action (() -> n)
               | Forever (Flow r ()) (() -> n)
               | Fork (Flow r ()) (ThreadId -> n)
               | MakeGenerator (StdGen -> n)
               | ModifyState (FlowState -> FlowState) (() -> n)
               | KillThread ThreadId (() -> n)
               | ReadState (FlowState -> n)
               | Replicate Int (Flow r ()) (() -> n)
               | Until (Flow r ()) (FlowState -> Bool) (Flow r ()) (() -> n)
               | Wait Int (() -> n)
               | WriteState FlowState (() -> n)
               deriving Functor

data FlowState = FlowState { _characterHitsCount
                           , _trackRowsCount :: Int
                           , _characterMoved
                           , _characterStuck
                           , _paused
                           , _started
                           , _trackBodyPassed :: Bool
                           , _flowThreadId
                           , _sessionThreadId :: Maybe ThreadId
                           , _trackName :: String
                           } deriving (Eq, Show)

data Action = FeedNextTrackCycle StdGen
            | FeedNextTrackPiece
            | Refresh StdGen
            | ReturnCharacter
            | Signal Signal


makeFieldsNoPrefix ''FlowState


dispatch :: Action -> Flow r ()
dispatch action = Free (Dispatch action Pure)

forever' :: Flow r () -> Flow r ()
forever' flow = Free (Forever flow Pure)

fork :: Flow r () -> Flow r ThreadId
fork flow = Free (Fork flow Pure)

makeGenerator :: Flow r StdGen
makeGenerator = Free (MakeGenerator Pure)

modifyState :: (FlowState -> FlowState) -> Flow r ()
modifyState f = Free (ModifyState f Pure)

killThread' :: ThreadId -> Flow r ()
killThread' threadId = Free (KillThread threadId Pure)

readState :: Flow r FlowState
readState = Free (ReadState Pure)

replicate' :: Int -> Flow r () -> Flow r ()
replicate' times flow = Free (Replicate times flow Pure)

until' :: Flow r () -> (FlowState -> Bool) -> Flow r () -> Flow r ()
until' suspendFlow f flow = Free (Until suspendFlow f flow Pure)

wait :: Int -> Flow n ()
wait time = Free (Wait time Pure)

writeState :: FlowState -> Flow n ()
writeState globalState = Free (WriteState globalState Pure)

initialiseFlowState :: Monad m => ReaderT Configuration m FlowState
initialiseFlowState = do
    trackName' <- asks (^. preferences . trackName)
    return $ FlowState 0
                       0
                       False
                       False
                       False
                       True
                       False
                       Nothing
                       Nothing
                       trackName'
