{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}


module Driver.Driver.Brick where


import Core.Character.Character
import Core.Configuration.Configuration
import Core.Port.Driver
import Core.Track.Character.Character
import Core.Track.Configuration.Configuration
import Core.Track.Track

import Driver.Renderer.Cnsl

import Brick
import Brick.BChan
import Brick.Widgets.Center
import Control.Concurrent
import Control.Lens
import Control.Lens.Extras
import Control.Monad
import Control.Monad.Free
import Control.Monad.Reader
import Data.IORef
import System.Random
import qualified Graphics.Vty as Vty
import Data.Maybe

import Core.Signal.Signal

import Core.Script.Track

import Brick.Keybindings
import System.Directory
import qualified Data.List as List
import qualified Data.Map.NonEmpty as Map
import qualified Data.Text as Text


data Page = RacePage | KBindsPage deriving (Bounded, Enum, Eq, Ord)
instance Show Page where
    show RacePage = "Start"
    show KBindsPage = "Key Bindings"

data GlobState = GlobState { _currTrackCycPiecesCnt
                           , _currTrackCycRemRowsCnt :: Int
                           , _flowThreadId
                           , _sessThreadId :: Maybe ThreadId
                           , _paused
                           , _started :: Bool
                           }

data LocState = LocState { _charPos :: Position
                         , _track :: State
                         , _actPage :: Maybe Page
                         , _kBinds :: [(Signal, [Binding])]
                         , _kBindsAdded :: Bool
                         , _slctMenuItemIx :: Int
                         }


makeFieldsNoPrefix ''LocState
makeFieldsNoPrefix ''GlobState


data Brick
instance Driver Brick where
    run _ = do
        currTrackName <- asks (^. preferences . trackName)
        opts <- asks _options
        trackPieceCap <- asks (^. preferences
                               . trackPieceCapacity
                               . to fromIntegral
                              )
        charProgS <- asks (^. options . characterProgressSpeed)
        (currTrackState, evChan, globStateRef) <- liftIO $ do
            gen <- newStdGen
            let currTrack = tracks' Map.! currTrackName
                currTrackCyc = getCycle currTrack
                currTrackInf = is _Free currTrackCyc
                interpret'' = configure opts
                currTrackState = interpret'' currTrack gen
                currTrackRowsCnt = currTrackState ^. rows . to length
                currTrackPiecesCnt = currTrackRowsCnt `div` trackPieceCap
                currTrackRemRowsCnt = currTrackRowsCnt
                                    `mod` trackPieceCap
                                    + (currTrackPiecesCnt - 1)
                trackRowTime = round $ 1 / charProgS * 1000000
            evChan <- newBChan 10
            globStateRef <- newIORef initGlobState
            let feedTrackRow = do
                    GlobState {..} <- readIORef globStateRef
                    threadDelay trackRowTime
                    if not _paused
                    then writeBChan evChan FeedTrackRow
                    else feedTrackRow
                start = do
                    replicateM_ currTrackPiecesCnt $ do
                        replicateM_ trackPieceCap feedTrackRow
                        writeBChan evChan FeedTrackPiece
                        writeBChan evChan ReturnChar
                    replicateM_ currTrackRemRowsCnt feedTrackRow
                    if currTrackInf
                    then forever $ do
                        writeBChan evChan FeedTrackCyc
                        writeBChan evChan ReturnChar
                        GlobState {..} <- readIORef globStateRef
                        replicateM_ _currTrackCycPiecesCnt $ do
                            replicateM_ trackPieceCap feedTrackRow
                            writeBChan evChan FeedTrackPiece
                            writeBChan evChan ReturnChar
                        replicateM_ _currTrackCycRemRowsCnt feedTrackRow
                    else do
                        modifyIORef globStateRef (& paused .~ True)
                        writeBChan evChan Fin
                        writeBChan evChan Start
            flowThreadId' <- forkIO . forever $ do
                GlobState {..} <- readIORef globStateRef
                unless _started $ do
                    maybe (return ()) killThread _sessThreadId
                    sessThreadId' <- forkIO start
                    modifyIORef globStateRef
                                $ (& started .~ True)
                                  . (& sessThreadId .~ Just sessThreadId')
            modifyIORef globStateRef (& flowThreadId .~ Just flowThreadId')
            return (currTrackState, evChan, globStateRef)
        app' <- app globStateRef
        kBinds' <- liftIO getKBinds
        initLocState' <- initLocState currTrackState kBinds'
        liftIO $ do
            let bldVty = Vty.mkVty Vty.defaultConfig
            initVty <- bldVty
            void $ customMain initVty bldVty (Just evChan) app' initLocState'

data FlowEvent = FeedTrackRow
               | FeedTrackPiece
               | FeedTrackCyc
               | Fin
               | ReturnChar
               | Start


app :: IORef GlobState -> ReaderT Configuration IO (App LocState FlowEvent ())
app globStateRef = do
    draw' <- draw
    hndlEv' <- hndlEv globStateRef
    return $ App { appDraw = draw'
                 , appChooseCursor = neverShowCursor
                 , appHandleEvent = hndlEv'
                 , appStartEvent = return ()
                 , appAttrMap = const $ attrMap Vty.defAttr []
                 }

draw :: ReaderT Configuration IO (LocState -> [Widget ()])
draw = do
    trackPieceCap <- asks (^. preferences
                           . trackPieceCapacity
                           . to fromIntegral
                          )
    let f (LocState charPos' trackState (Just RacePage) _ _ _)
            =
            let trackPiece = trackState ^. rows . to (take trackPieceCap)
            in
                [ center . str
                         . show
                         . RndredTrackLines
                         $ reflect charPos' trackPiece
                ]
        f (LocState _ _ (Just KBindsPage) kBinds' kBindsAdded' slctMenuItemIx')
            = case sig' of
                  StrafeLeft
                      ->
                      [ hCenter (str $ show KBindsPage)
                        <=> center (kBindWid StrafeLeft True kBindsAdded'
                                    <=> kBindWid StrafeRight False False
                                   )
                      ]
                  StrafeRight
                      ->
                      [ hCenter (str $ show KBindsPage)
                        <=> center (kBindWid StrafeLeft False False
                                    <=> kBindWid StrafeRight True kBindsAdded'
                                   )
                      ]
          where
            comSepSlctSigKBinds sig = do
                unqSlctSigKBinds <- (map ppBinding . List.nub . snd . (kBinds' !!))
                                 <$> List.findIndex ((== sig) . fst) kBinds'
                return . Text.unpack
                       . Text.concat
                       . (++ unqSlctSigKBinds ^? _last . to (: []) ^. non [])
                       $ map (<> ", ") (unqSlctSigKBinds ^? _init ^. non [])
            kBindWid sig True True = str
                                   $ "* "
                                   ++ show sig
                                   ++ ": "
                                   ++ fromJust (comSepSlctSigKBinds sig)
                                   ++ if null . fromJust
                                              $ comSepSlctSigKBinds sig
                                      then "?"
                                      else ", ?"
            kBindWid sig True False = str
                                    $ "* "
                                    ++ show sig
                                    ++ ": "
                                    ++ fromJust (comSepSlctSigKBinds sig)
                                    ++ "   "
            kBindWid sig False _ = str
                                 $ "  "
                                 ++ show sig
                                 ++ ": "
                                 ++ fromJust (comSepSlctSigKBinds sig)
                                 ++ "   "
            sig' = toEnum slctMenuItemIx' :: Signal
        f (LocState _ _ Nothing _ _ slctMenuItemIx')
            =
            case slctPage of
                RacePage -> slctRacePage
                KBindsPage -> slctKBindsPage
          where
            slctKBindsPage
                =
                [ hCenter (str "Main")
                  <=> center (str ("  " ++ show RacePage)
                              <=> str ("* " ++ show KBindsPage)
                             )
                ]
            slctRacePage
                =
                [ hCenter (str "Main")
                  <=> center (str ("* " ++ show RacePage)
                              <=> str ("  " ++ show KBindsPage)
                             )
                ]
            slctPage = toEnum slctMenuItemIx'
    return f

hndlEv :: IORef GlobState
       -> ReaderT Configuration
                  IO
                  (BrickEvent () FlowEvent -> EventM () LocState ())
hndlEv globStateRef = do
    currTrackName <- asks (^. preferences . trackName)
    opts <- asks _options
    trackPieceCap <- asks (^. preferences
                           . trackPieceCapacity
                           . to fromIntegral
                          )
    let interpretFrom' = configureFrom opts
        currTrack = tracks' Map.! currTrackName
        initCharPos = runReader spawn opts
        interpret'' = configure opts
    return $ \case
                 AppEvent FeedTrackRow -> do
                     charPos %= progress
                 AppEvent FeedTrackPiece -> do
                     track . rows %= drop (trackPieceCap - 1)
                 AppEvent FeedTrackCyc -> do
                     gen <- newStdGen
                     prevTrackState <- use track
                     let currTrackCyc = getCycle currTrack
                         initTrackState = prevTrackState
                                        & rows %~ (pure . head)
                         contTrackState = interpretFrom' initTrackState
                                                         currTrackCyc
                                                         gen
                     track .= contTrackState
                     track . rows %= reverse
                     let currTrackCycPiecesCnt' = contTrackState
                                                  ^. rows
                                                  . to length
                                                `div` trackPieceCap
                         currTrackCycRemRowsCnt' = contTrackState
                                                   ^. rows
                                                   . to length
                                                 `mod` trackPieceCap
                                                 + (currTrackCycPiecesCnt' - 1)
                     liftIO $ do
                         modifyIORef globStateRef
                                     $ (& currTrackCycPiecesCnt
                                        .~ currTrackCycPiecesCnt'
                                       )
                                       . (& currTrackCycRemRowsCnt
                                          .~ currTrackCycRemRowsCnt'
                                         )
                 AppEvent Fin -> do
                     actPage .= Nothing
                 AppEvent ReturnChar -> do
                     charPos %= backtrack
                 AppEvent Start -> do
                     gen <- newStdGen
                     let track' = interpret'' currTrack gen
                     charPos .= initCharPos
                     track .= track'
                     track . rows %= reverse
                 VtyEvent (Vty.EvKey k mods) -> do
                     let kBind = binding k mods
                     if | k == (Vty.KChar 'q') -> do
                            kBinds' <- (& each
                                        . _2
                                        %~ Text.intercalate "," . map ppBinding
                                       )
                                    <$> use kBinds
                            liftIO $ do
                                GlobState {..} <- readIORef globStateRef
                                maybe (return ()) killThread _flowThreadId
                                maybe (return ()) killThread _sessThreadId
                                kBindsExist <- doesFileExist kBindsSavFileName
                                when kBindsExist $ removeFile kBindsSavFileName
                                writeFile kBindsSavFileName $ show kBinds'
                            halt
                        | k == Vty.KBS -> do
                            actPage' <- use actPage
                            case actPage' of
                                Just KBindsPage -> do
                                    kBinds' <- use kBinds
                                    slctMenuItemIx' <- use slctMenuItemIx
                                    let sig = toEnum slctMenuItemIx' :: Signal
                                        kBindIx = List.findIndex ((== sig) . fst)
                                                                 kBinds'
                                    case kBindIx of
                                        Just kBindIx' -> do
                                            kBinds . ix kBindIx' . _2 .= []
                                        Nothing -> do
                                            return ()
                                Nothing -> do
                                    kBinds .= defKBinds
                                _ -> do
                                    return ()

                        | k == Vty.KEnter -> do
                            actPage' <- use actPage
                            case actPage' of
                                Just KBindsPage -> do
                                    kBindsAdded %= not
                                Nothing -> do
                                    slctPage' <- toEnum <$> use slctMenuItemIx
                                    actPage .= Just slctPage'
                                    when (slctPage' == RacePage) $ do
                                        liftIO $ do
                                            modifyIORef globStateRef
                                                        $ (& paused .~ False)
                                                          . (& started .~ False)
                                    slctMenuItemIx .= 0
                                _ -> do
                                    return ()
                        | k == Vty.KEsc -> do
                            actPage' <- use actPage
                            if actPage' == Nothing
                            then do
                                slctPage' <- toEnum <$> use slctMenuItemIx
                                actPage .= Just slctPage'
                            else do
                                actPage .= Nothing
                            slctPage' <- toEnum <$> use slctMenuItemIx
                            when (slctPage' == RacePage) . liftIO $ do
                                modifyIORef globStateRef (& paused .~ False)
                        | k `elem` [Vty.KChar 'w', Vty.KUp] -> do
                            slctPage' :: Page <- toEnum <$> use slctMenuItemIx
                            when (slctPage' > minBound) $ slctMenuItemIx %= pred
                        | k `elem` [Vty.KChar 's', Vty.KDown] -> do
                            slctPage' :: Page <- toEnum <$> use slctMenuItemIx
                            when (slctPage' < maxBound) $ slctMenuItemIx %= succ
                        | otherwise -> do
                            kBinds' <- use kBinds
                            addKBinds' <- use kBindsAdded
                            if addKBinds'
                            then do
                                pageItemIx' <- use slctMenuItemIx
                                let sig = toEnum pageItemIx' :: Signal
                                    kBindIx = List.findIndex ((== sig) . fst)
                                                             kBinds'
                                case kBindIx of
                                    Just kBindIx' -> do
                                        kBinds %= (^.. traversed
                                                   . to (& _2
                                                         %~ filter (/= kBind)
                                                        )
                                                  )
                                        kBinds . ix kBindIx'
                                               . _2
                                               %= (++ [kBind])
                                    Nothing -> do
                                        return ()
                            else case List.findIndex (elem kBind . snd) kBinds' of
                                Just sigIx -> do
                                    let sig = fst $ kBinds' !! sigIx
                                    GlobState {..} <- liftIO
                                                    $ readIORef globStateRef
                                    when (not _paused) $ do
                                        charPos %= (`runReader` opts)
                                                   . strafe (signalToSide sig)
                                Nothing -> do
                                    return ()
                 _ -> do
                     return ()

initGlobState ::  GlobState
initGlobState = GlobState 0 0 Nothing Nothing True True

initLocState :: State
             -> [(Signal, [Binding])]
             -> ReaderT Configuration IO LocState
initLocState trackState kBinds' = do
    opts <- asks _options
    let initCharPos = runReader spawn opts
    return $ LocState initCharPos
                      (trackState & rows %~ reverse)
                      Nothing
                      kBinds'
                      False
                      0

defKBinds :: [(Signal, [Binding])]
defKBinds
   =
   [ (StrafeLeft, [bind 'a', bind Vty.KLeft])
   , (StrafeRight, [bind 'd', bind Vty.KRight])
   ]

getKBinds :: IO [(Signal, [Binding])]
getKBinds = do
    kBindsExist <- doesFileExist kBindsSavFileName
    if kBindsExist
    then do
        kBinds' <- (& sequence)
                   . (& each %~ sequence)
                   . (& each . _2
                               %~ ((\case
                                        BindingList bl -> bl
                                        Unbound -> []
                                   ) <$>
                                  )
                                  . parseBindingList
                     )
                   . read @[(Signal, Text.Text)]
                <$> readFile kBindsSavFileName
        return $ case kBinds' of
                     Right kBinds'' -> kBinds''
                     Left _ -> defKBinds
    else return defKBinds

kBindsSavFileName :: String
kBindsSavFileName = ".k-binds.sav"
