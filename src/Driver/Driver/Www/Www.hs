{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}


module Driver.Driver.Www.Www where


import Core.Port.Driver

import Miso hiding ((<#), JSM)

import Core.Character.Character
import Core.Configuration.Configuration
import Core.Flow.Flow
import Core.Core
import Core.Signal.Signal
import Core.Track.Configuration.Configuration
import Core.Track.Character.Character
import Core.Track.Track

import Core.Script.Flow.Flow1
import Core.Script.Track

import Driver.Driver.Www.View.Main
import Driver.Www.Comm

import Control.Concurrent
import Control.Lens hiding ((#), view)
import Control.Monad.Free
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Data.Maybe
import GHCJS.DOM
import GHCJS.DOM.HTMLCanvasElement
import GHCJS.DOM.Node
import GHCJS.DOM.Types
import GHCJS.DOM.Window hiding (getLocalStorage)
#ifndef __GHCJS__
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.Warp
#endif
import Numeric.Natural
import "ghcjs-dom" GHCJS.DOM.Document
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.Random as Rand


import Driver.Driver.Www.Js

import Driver.Parser.Aeson ()


data WwwState = WwwState { _drawTrackPiece :: [[Cell]] -> JSM ()
                         , _moveChar :: Int -> Int -> JSM ()
                         , _render' :: JSM ()
                         }


makeFieldsNoPrefix ''WwwState


data Www
instance Driver Www where
    run _ _ = do
        conf <- ask
        initGlobState <- initialiseGlobalState
        (globStateRef, wwwStateRef) <- liftIO $ do
            threadDelay 20000
            globStateRef <- newIORef initGlobState
            wwwStateRef <- newIORef initWwwState
            return (globStateRef, wwwStateRef)
        flowSub <- mkFlowSub globStateRef wwwStateRef
        initTrackState <- initialiseTrackState
        initCoreState <- initialiseCoreState initTrackState
        lift $ do
            let events = defaultEvents
                initialAction = MisoAct Initialise
                logLevel = Off
                model = Mdl initCoreState initGlobState Map.empty
                mountPoint = Nothing
                subs = [flowSub, keyboardSub $ MisoAct . HandleKs]
                update = fromTransition
                       . (`runReaderT` conf)
                       . upd globStateRef wwwStateRef
                view = mainView
            runApp $ startApp App {..}

cellToPos :: Configuration -> Int -> Int -> (Int, Int)
cellToPos conf row col = (x, z)
  where
    trackW = conf ^. options . trackWidth . to (fromIntegral @Natural @Int)
    halfTrackW = trackW `div` 2
    x = negate $ halfTrackW - col
    trackPieceCap = conf
                  ^. preferences
                  . trackPieceCapacity
                  . to (fromIntegral @Natural @Int)
    halfTrackPieceCap = trackPieceCap `div` 2
    z = halfTrackPieceCap - row

cellToCol :: Cell -> String
cellToCol Character = "green"
cellToCol DeadEnemy = "brown"
cellToCol LivingEnemy = "red"
cellToCol Obstacle = "white"
cellToCol Pass = "gray"
cellToCol TrailPart = "white"

cellToGeom :: Cell -> JSM (Maybe JSVal)
cellToGeom cell = do
    if | cell `elem` [Character, DeadEnemy, LivingEnemy]
       -> Just <$> newSphereGeom 0.5
       | cell `elem` [Pass, TrailPart]
       -> Just <$> newSphereGeom 0.1
       | cell == Obstacle
       -> Just <$> newBoxGeom 1 1 1
       | otherwise
       -> return Nothing

initWwwState :: WwwState
initWwwState = WwwState (const $ return ()) (\_ _ -> return ()) (return ())

keyCodeToSig :: Int -> Maybe PlayerSignal
keyCodeToSig 72 = Just StrafeLeft
keyCodeToSig 74 = Just SwingLeft
keyCodeToSig 75 = Just SwingRight
keyCodeToSig 76 = Just StrafeRight
keyCodeToSig _ = Nothing

mkFlowSub :: Monad m
          => IORef FlowState
          -> IORef WwwState
          -> ReaderT Configuration m (Sub Act)
mkFlowSub globStateRef _ = do
    currFlow <- flow1
    return $ \sink -> void . liftIO . forkIO $ do
        threadDelay 100000
        runFlow globStateRef sink currFlow

runFlow :: IORef FlowState -> (Act -> IO ()) -> Flow r n -> IO ()
runFlow _ _ (Pure _) = pure ()
runFlow globStateRef sink (Free (Dispatch act next)) = do
    sink $ Act act
    runFlow globStateRef sink $ next ()
runFlow globStateRef sink (Free (Forever flow' next)) = do
    _ <- forever $ runFlow globStateRef sink flow'
    runFlow globStateRef sink $ next ()
runFlow globStateRef sink (Free (Fork flow' next)) = do
    threadId <- forkIO $ runFlow globStateRef sink flow'
    runFlow globStateRef sink $ next threadId
runFlow globStateRef sink (Free (MakeGenerator next)) = do
    gen <- Rand.newStdGen 
    runFlow globStateRef sink $ next gen
runFlow globStateRef sink (Free (ModifyState f next)) = do
    modifyIORef globStateRef f
    runFlow globStateRef sink $ next ()
runFlow globStateRef sink (Free (KillThread threadId next)) = do
    killThread threadId 
    runFlow globStateRef sink $ next ()
runFlow globStateRef sink (Free (ReadState next)) = do
    globState <- readIORef globStateRef
    runFlow globStateRef sink $ next globState
runFlow globStateRef sink (Free (Replicate n flow' next)) = do
    replicateM_ n (runFlow globStateRef sink flow')
    runFlow globStateRef sink $ next ()
runFlow globStateRef sink (Free (Until suspFlow f flow' next)) = do
    globState <- readIORef globStateRef
    if not $ f globState
    then do
        runFlow globStateRef sink suspFlow
        runFlow globStateRef sink $ until' suspFlow f flow'
        runFlow globStateRef sink $ next ()
    else do
        runFlow globStateRef sink flow'
        runFlow globStateRef sink $ next ()
runFlow globStateRef sink (Free (Wait t next)) = do
    threadDelay t
    runFlow globStateRef sink $ next ()
runFlow globStateRef sink (Free (WriteState globState next)) = do
    writeIORef globStateRef globState
    runFlow globStateRef sink $ next ()

upd :: IORef FlowState
    -> IORef WwwState
    -> Act -> ReaderT Configuration (Transition Act Mdl) ()
upd _ _ (MisoAct DoNothing) = pure ()
upd _ wwwStateRef (Act FeedNextTrackPiece) = do
    trackPieceCap <- asks (^. preferences
                           . trackPieceCapacity
                           . to (fromIntegral @Natural @Int)
                          )
    lift $ do
        core . track . rows %= drop (trackPieceCap - 1)
        core . track
                  . rows
                  %= (\(part, rest)
                      ->
                      part ++ if null part then [] else [last part] ++ rest
                     )
                     . splitAt (trackPieceCap - 1)
        currTrackPiece <- use (core . track . rows . to (take trackPieceCap))
        scheduleIO_ $ do
            WwwState {..} <- liftIO $ readIORef wwwStateRef
            _drawTrackPiece currTrackPiece
upd globStateRef wwwStateRef (Act (FeedNextTrackCycle gen)) = do
    conf <- ask
    lift $ do
        prevTrackState <- use (core . track)
        let currTrackName = prevTrackState ^. name
            currTrack = tracks' Map.! currTrackName
            initTrackState = prevTrackState & rows %~ (pure . last)
            currTrackCyc = getCycle currTrack
            interpretFrom' = configureFrom conf
            contTrackState = interpretFrom' initTrackState currTrackCyc gen
                           & rows %~ reverse . init
        core . track .= contTrackState
        let trackPieceCap = conf
                          ^. preferences
                          . trackPieceCapacity
                          . to (fromIntegral @Natural @Int)
        core . track
             . rows
             %= (\(part, rest) -> part ++ [last part] ++ rest)
                . splitAt (trackPieceCap - 1)
        currTrackPiece <- use (core . track . rows . to (take trackPieceCap))
        scheduleIO_ $ do
            liftIO $ do
                modifyIORef globStateRef
                            $ (& trackRowsCount
                               .~ contTrackState
                               ^. rows
                               . to length
                              )
            WwwState {..} <- liftIO $ readIORef wwwStateRef
            _drawTrackPiece currTrackPiece
            _render'
upd globStateRef wwwStateRef (MisoAct (HandleKs ks)) = do
    conf <- ask
    trackPieceCap <- asks (^. preferences
                           . trackPieceCapacity
                           . to (fromIntegral @Natural @Int)
                          )
    kBinds' <- lift $ use kBinds
    let currPlayerSigs = Map.keys $ Map.filter (`elem` (Set.toList ks)) kBinds'
        playerWantsPause = Pause `elem` currPlayerSigs 
        playerWantsQuit = Quit `elem` currPlayerSigs
    if | playerWantsPause -> lift . scheduleIO . liftIO $ do
        modifyIORef globStateRef (& paused %~ not)
        return $ MisoAct TogglePauseMode
       | playerWantsQuit -> lift . scheduleIO . liftIO $ do
        FlowState {..} <- readIORef globStateRef
        maybe (return ()) killThread _flowThreadId
        maybe (return ()) killThread _sessionThreadId
        return $ MisoAct Pause'
       | otherwise -> do
        paused' <- use (flow . paused)
        trackPassed <- use (core . track . rows . to ((== 0) . length))
        when (not paused' && not trackPassed) $ do
            let playerWantsStrafe = StrafeLeft `elem` currPlayerSigs
                                  || StrafeRight `elem` currPlayerSigs
                playerWantsSwing = SwingLeft `elem` currPlayerSigs
                                 || SwingRight `elem` currPlayerSigs
            if | playerWantsStrafe -> do
                let currStrafePlayerSig | StrafeLeft `elem` currPlayerSigs
                                        = StrafeLeft
                                        | StrafeRight `elem` currPlayerSigs
                                        = StrafeRight
                                        | otherwise
                                        = error "undefined strafe signal"
                    currStrafeSig = PlayerSignal currStrafePlayerSig
                prevCoreState <- lift $ use core
                nextCoreState <- reflect currStrafeSig prevCoreState
                lift $ do
                    prevCharCol <- use (core
                                        . character
                                        . position
                                        . unPosition
                                        . _2
                                       )
                    core .= nextCoreState
                    nextCharCol <- use (core
                                        . character
                                        . position
                                        . unPosition
                                        . _2
                                       )
                    (charRow, charCol) <- bimap (fromIntegral @Natural @Int)
                                                (fromIntegral @Natural @Int)
                                       <$> use (core
                                                . character
                                                . position
                                                . unPosition
                                               )
                    charHP <- use (core . character . hitPoints)
                    scheduleIO_ $ do
                        WwwState {..} <- liftIO $ do
                            let charMoved = prevCharCol /= nextCharCol
                            modifyIORef globStateRef
                                        (& characterStuck .~ not charMoved)
                            let charDead = charHP == 0
                            when charDead $ do
                                modifyIORef globStateRef
                                            $ (& characterStuck .~ False)
                                              . (& paused .~ False)
                                              . (& started .~ False)
                            readIORef wwwStateRef
                        let (charXPos, charZPos) = cellToPos conf
                                                             charRow
                                                             charCol
                        _moveChar charXPos charZPos
                        _render'
                | playerWantsSwing -> do
                    let currSwingPlayerSig | SwingLeft `elem` currPlayerSigs
                                           = SwingLeft
                                           | SwingRight `elem` currPlayerSigs
                                           = SwingRight
                                           | otherwise
                                           = error "undefined swing signal"
                        currSwingSig = PlayerSignal currSwingPlayerSig
                    prevCoreState <- lift $ use core
                    nextCoreState <- reflect currSwingSig prevCoreState
                    lift $ do
                        core .= nextCoreState
                        currTrackPiece <- use (core
                                               . track
                                               . rows
                                               . to (take trackPieceCap)
                                              )
                        scheduleIO_ $ do
                            WwwState {..} <- liftIO $ readIORef wwwStateRef
                            _drawTrackPiece currTrackPiece
                            _render'
                | otherwise -> do
                    return ()
upd _ wwwStateRef (MisoAct Initialise) = do
    conf <- ask
    trackPartCap <- asks (^. preferences
                          . trackPieceCapacity
                          . to (fromIntegral @Natural @Int)
                         )
    lift $ do
        currTrackPart <- use (core . track . rows . to (take trackPartCap))
        scheduleIO $ do
            liftIO $ threadDelay 100000
            doc <- currentDocumentUnchecked
            body <- getBodyUnchecked doc
            canv <- uncheckedCastTo HTMLCanvasElement
                 <$> createElement doc ("canvas" :: String)
            _ <- appendChild body canv
            jsCanv <- toJSVal canv
            win <- currentWindowUnchecked
            innH <- fromIntegral @Int @Float <$> getInnerHeight win
            innW <- fromIntegral @Int @Float <$> getInnerWidth win
            scene <- newScene
            trackGrp <- newGroup
            setName' trackGrp "track"
            add scene trackGrp
            cam <- newCam 75 (innW / innH) 0.1 1000
            setAxRot cam "x" $ -pi / 2
            setAxPos cam "y" $ 20
            setAxRot cam "z" $ 0
            let halfTrackPartCap = trackPartCap `div` 2
            renderer <- newWebglRenderer jsCanv
            setSize renderer innW innH
            let drawTrackCell :: Maybe String
                              -> Int
                              -> Int
                              -> Cell
                              -> JSM (Maybe JSVal)
                drawTrackCell groupName x z cell = do
                    geom <- cellToGeom cell
                    case geom of
                        Just geom' -> do
                            matCol <- toJSVal $ cellToCol cell
                            mat <- newLineBasMat matCol
                            wireframe <- newEdgesGeom geom'
                            mesh <- newLineSegs wireframe mat
                            setAxPos mesh "x" $ x
                            setAxPos mesh "z" $ z
                            case groupName of
                                Just groupName' -> do
                                    group' <- getObjByName scene
                                           =<< toJSVal groupName'
                                    add group' mesh
                                Nothing -> do
                                    add scene mesh
                            return $ Just mesh
                        Nothing -> do
                            return Nothing
                drawTrackPiece' piece = do
                    clearChildren trackGrp
                    let ixedTrackPart = zip [0..length piece - 1] piece
                    forM_ ixedTrackPart $ \(cellRow, trackRow) -> do
                        let ixedTrackRow = zip [0..length trackRow - 1] trackRow
                        forM_ ixedTrackRow $ \(cellCol, cell) -> do
                            let (cellXPos, cellZPos) = cellToPos conf
                                                                 cellRow
                                                                 cellCol
                                cellChar = cell == Character
                            when (not cellChar) $ do
                                _ <- drawTrackCell (Just "track")
                                                   cellXPos
                                                   cellZPos
                                                   cell
                                return ()
                render'' = render renderer scene cam
            char <- drawTrackCell Nothing (0 :: Int) halfTrackPartCap Character
            case char of
                Just char' -> do
                    let moveChar' x z = do
                            setAxPos char' "x" $ x
                            setAxPos char' "z" $ z
                    liftIO . writeIORef wwwStateRef
                           $ WwwState drawTrackPiece' moveChar' render''
                    drawTrackPiece' currTrackPart
                    render''
                Nothing -> do
                    return ()
            kBinds' <- either (const defKBinds) id
                    <$> getLocalStorage @(Map.Map PlayerSignal Int) "k-binds"
            return $ MisoAct (SetKBinds kBinds')
upd _ _ (MisoAct Pause') = lift $ flow . paused .= True 
upd globStateRef wwwStateRef (Act (Refresh gen)) = do
    conf <- ask
    currTrackName <- asks (^. preferences . trackName)
    trackPieceCap <- asks (^. preferences
                           . trackPieceCapacity
                           . to (fromIntegral @Natural @Int)
                          )
    let currTrack = tracks' Map.! currTrackName
        interpret'' = configure conf
        initTrackState = interpret'' currTrack gen & rows %~ reverse
    initCoreState <- initialiseCoreState initTrackState
    lift $ do
        core .= initCoreState
        core . track
             . rows
             %= (\(part, rest)
                 -> part ++ if null part then [] else [last part] ++ rest
                )
                . splitAt (trackPieceCap - 1)
        currTrackPiece <- use (core . track . rows . to (take trackPieceCap))
        scheduleIO_ $ do
            WwwState {..} <- liftIO $ do
                modifyIORef globStateRef
                            (& trackRowsCount
                             .~ initTrackState
                             ^. rows
                             . to length
                            )
                readIORef wwwStateRef
            _drawTrackPiece currTrackPiece
upd _ wwwStateRef (Act ReturnCharacter) = do
    conf <- ask
    lift $ do
        core . character . position %= backtrack
        (charRow, charCol) <- bimap (fromIntegral @Natural @Int)
                                    (fromIntegral @Natural @Int)
                           <$> use (core . character . position . unPosition)
        scheduleIO_ $ do
            let (charXPos, charZPos) = cellToPos conf charRow charCol
            WwwState {..} <- liftIO $ readIORef wwwStateRef
            _moveChar charXPos charZPos
            _render'
upd _ _ (MisoAct (SetKBinds kBinds')) = lift $ kBinds .= kBinds'
upd globStateRef wwwStateRef (Act (Signal (FlowSignal Progress))) = do
    conf <- ask
    let sig = FlowSignal Progress
    prevCoreState <- lift $ use core
    nextCoreState <- reflect sig prevCoreState
    lift $ do
        prevCharRow <- use (core . character . position . unPosition . _1)
        core .= nextCoreState
        nextCharRow <- use (core . character . position . unPosition . _1)
        (charRow, charCol) <- bimap (fromIntegral @Natural @Int)
                                    (fromIntegral @Natural @Int)
                           <$> use (core . character . position . unPosition)
        cellAheadChar <- gets (^? core
                               . track
                               . rows
                               . ix (charRow + 1)
                               . ix charCol
                              )
        charHP <- use (core . character . hitPoints)
        scheduleIO_ $ do
            let charProgressed = prevCharRow /= nextCharRow
                obstAheadChar = isJust $ isObstacle <$> cellAheadChar
                charDead = charHP == 0
                charHit = not charProgressed && obstAheadChar
            liftIO $ do
                modifyIORef globStateRef (& characterStuck .~ charHit)
                when charDead $ do
                    modifyIORef globStateRef $ (& started .~ False)
                                               . (& paused .~ False)
                                               . (& characterStuck .~ False)
                when charHit $ do
                    modifyIORef globStateRef (& characterHitsCount %~ (+ 1))
        scheduleIO_ $ do
            let (charXPos, charZPos) = cellToPos conf charRow charCol
            WwwState {..} <- liftIO $ readIORef wwwStateRef
            _moveChar charXPos charZPos
            _render'
upd _ _ (Act (Signal (PlayerSignal _))) = pure ()
upd _ _ (MisoAct TogglePauseMode) = flow . paused %= not

#ifdef __GHCJS__
runApp :: IO () -> IO ()
runApp = id
#else
runApp :: JSM () -> IO ()
runApp f = debugOr 8000 (f >> syncPoint) jsaddleApp
#endif

defKBinds :: Map.Map PlayerSignal Int
defKBinds
    =
    Map.fromList [ (StrafeLeft, 72)
                 , (SwingLeft, 74)
                 , (SwingRight, 75)
                 , (StrafeRight, 76)
                 , (Pause, 27)
                 , (Quit, 81)
                 ]
