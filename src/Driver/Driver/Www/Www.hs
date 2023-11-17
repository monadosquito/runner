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
import GHCJS.DOM.Window
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.Warp
import Numeric.Natural
import "ghcjs-dom" GHCJS.DOM.Document
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.Random as Rand


#ifdef __GHCJS__
type IO' = IO
#else
type IO' = JSM
#endif


data WwwState = WwwState { _drawTrackPiece :: [[Cell]] -> JSM ()
                         , _moveChar :: Int -> Int -> JSM ()
                         , _render :: JSM ()
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
                model = Mdl initCoreState initGlobState
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
    three <- jsg ("THREE" :: String)
    if | cell `elem` [Character, DeadEnemy, LivingEnemy]
       -> Just
          <$> (new (three ! ("SphereGeometry" :: String)) $ ([0.5] :: [Float]))
       | cell `elem` [Pass, TrailPart]
       -> Just
          <$> (new (three ! ("SphereGeometry" :: String)) $ ([0.1] :: [Float]))
       | cell == Obstacle
       -> Just
          <$> (new (three ! ("BoxGeometry" :: String))
          $ ((1, 1, 1) :: (Int, Int, Int)))
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
                      -> part ++ if null part then [] else [last part] ++ rest
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
            _render
upd globStateRef wwwStateRef (MisoAct (HandleKs ks)) = do
    conf <- ask
    if | 27 `elem` ks -> lift . scheduleIO . liftIO $ do
        modifyIORef globStateRef (& paused %~ not)
        return $ MisoAct TogglePauseMode
       | 81 `elem` ks -> lift . scheduleIO . liftIO $ do
        FlowState {..} <- readIORef globStateRef
        maybe (return ()) killThread _flowThreadId
        maybe (return ()) killThread _sessionThreadId
        return $ MisoAct Pause
       | otherwise -> do
        paused' <- use (flow . paused)
        trackPassed <- use (core . track . rows . to ((== 0) . length))
        when (not paused' && not trackPassed) $ do
            let currPlayerSigs = map keyCodeToSig $ Set.toList ks
                playerWantsStrafe = Just StrafeLeft `elem` currPlayerSigs
                                  || Just StrafeRight `elem` currPlayerSigs
                playerWantsSwing = Just SwingLeft `elem` currPlayerSigs
                                 || Just SwingRight `elem` currPlayerSigs
                trackPieceCap = conf
                              ^. preferences
                              . trackPieceCapacity
                              . to (fromIntegral @Natural @Int)
            if | playerWantsStrafe -> do
                let currStrafePlayerSig | Just StrafeLeft `elem` currPlayerSigs
                                        = Just StrafeLeft
                                        | Just StrafeRight `elem` currPlayerSigs
                                        = Just StrafeRight
                                        | otherwise
                                        = Nothing
                    currStrafeSig = PlayerSignal <$> currStrafePlayerSig
                case currStrafeSig of
                    Just currStrafeSig' -> do
                        prevCoreState <- lift $ use core
                        nextCoreState <- reflect currStrafeSig' prevCoreState
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
                            (charRow, charCol) <- bimap (fromIntegral @Natural
                                                                      @Int
                                                        )
                                                        (fromIntegral @Natural
                                                                      @Int
                                                        )
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
                                                (& characterStuck
                                                 .~ not charMoved
                                                )
                                    let charDead = charHP == 0
                                    when charDead $ do
                                        modifyIORef globStateRef
                                                    $ (& characterStuck
                                                       .~ False
                                                      )
                                                      . (& paused .~ False)
                                                      . (& started .~ False)
                                    readIORef wwwStateRef
                                let (charXPos, charZPos) = cellToPos conf
                                                                     charRow
                                                                     charCol
                                _moveChar charXPos charZPos
                                _render
                    Nothing -> do
                        return ()
                | playerWantsSwing -> do
                    let currSwingPlayerSig
                            | Just SwingLeft `elem` currPlayerSigs
                            = Just SwingLeft
                            | Just SwingRight `elem` currPlayerSigs
                            = Just SwingRight
                            | otherwise
                            = Nothing
                        currSwingSig = PlayerSignal <$> currSwingPlayerSig
                    case currSwingSig of
                        Just currSwingSig' -> do
                            prevCoreState <- lift $ use core
                            nextCoreState <- reflect currSwingSig' prevCoreState
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
                        Nothing -> do
                            return ()
                    return ()
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
        scheduleIO_ $ do
            liftIO $ threadDelay 100000
            doc <- currentDocumentUnchecked
            body <- getBodyUnchecked doc
            canv <- uncheckedCastTo HTMLCanvasElement
                 <$> createElement doc ("canvas" :: String)
            _ <- appendChild body canv
            jsCanv <- toJSVal canv
            emptyArgs <- obj
            setProp "canvas" jsCanv emptyArgs
            args <- toJSVal emptyArgs
            three <- jsg ("THREE" :: String)
            win <- currentWindowUnchecked
            innH <- fromIntegral @Int @Float <$> getInnerHeight win
            innW <- fromIntegral @Int @Float <$> getInnerWidth win
            scene <- new (three ! ("Scene" :: String)) $ ()
            trackGrp <- new (three ! ("Group" :: String)) $ ()
            trackGrp <# ("name" :: String) $ ("track" :: String)
            _ <- scene # ("add" :: String) $ trackGrp
            cam <- new (three ! ("PerspectiveCamera" :: String))
                $ (75 :: Int, innW / innH, 0.1 :: Float, 1000 :: Int)
            cam ! ("position" :: String) <# ("z" :: String) $ (0 :: Int)
            cam ! ("position" :: String) <# ("y" :: String) $ (20 :: Int)
            cam ! ("rotation" :: String) <# ("x" :: String) $ -pi @Float / 2
            let halfTrackPartCap = trackPartCap `div` 2
            renderer <- new (three ! ("WebGLRenderer" :: String)) args
            _ <- renderer # ("setSize" :: String) $ (innW, innH)
            let drawTrackCell :: Maybe String
                              -> Int
                              -> Int
                              -> Cell
                              -> JSM (Maybe JSVal)
                drawTrackCell groupName x z cell = do
                    geom <- cellToGeom cell
                    case geom of
                        Just geom' -> do
                            col <- toJSVal $ (cellToCol cell :: String)
                            matArgs <- obj
                            setProp "color" col matArgs
                            jsMatArgs <- toJSVal matArgs
                            mat <- new (three ! ("LineBasicMaterial" :: String))
                                       $ jsMatArgs
                            wireframe <- new (three ! ("EdgesGeometry" :: String))
                                             $ geom'
                            mesh <- new (three ! ("LineSegments" :: String))
                                        $ (wireframe, mat)
                            mesh ! ("position" :: String) <# ("x" :: String) $ x
                            mesh ! ("position" :: String) <# ("z" :: String) $ z
                            case groupName of
                                Just groupName' -> do
                                    group <- scene # ("getObjectByName" :: String)
                                                   $ [groupName']
                                    void $ group # ("add" :: String) $ mesh
                                Nothing -> do
                                    void $ scene # ("add" :: String) $ mesh
                            return $ Just mesh
                        Nothing -> do
                            return Nothing
                drawTrackPiece' piece = do
                    trackGrp ! ("children" :: String)
                             <# ("length" :: String)
                             $ (0 :: Int)
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
                render' = void $ renderer # ("render" :: String) $ (scene, cam)
            char <- drawTrackCell Nothing (0 :: Int) halfTrackPartCap Character
            case char of
                Just char' -> do
                    let moveChar' x z = do
                            char' ! ("position" :: String) <# ("x" :: String) $ x
                            char' ! ("position" :: String) <# ("z" :: String) $ z
                    liftIO . writeIORef wwwStateRef
                           $ WwwState drawTrackPiece' moveChar' render'
                    drawTrackPiece' currTrackPart
                    render'
                Nothing -> do
                    return ()
upd _ _ (MisoAct Pause) = lift $ do
    flow . paused .= True 
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
            _render
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
            _render
upd _ _ (Act (Signal (PlayerSignal _))) = pure ()
upd _ _ (MisoAct TogglePauseMode) = flow . paused %= not

#ifdef __GHCJS__
runApp :: IO () -> IO ()
runApp = id
#else
runApp :: JSM () -> IO ()
runApp f = debugOr 8000 (f >> syncPoint) jsaddleApp
#endif
