{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}


module Driver.Driver.Brick where

#ifndef __GHCJS__
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
import qualified Data.Map as Map
import qualified Data.Text as Text

import Core.Port.Parser

import Data.Proxy
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.List as NEList
import Text.Read hiding (lift)

import Core.Core
import Core.Flow.Flow
import Core.Script.Flow.Flow1

import Numeric.Natural


data Page = RacePage
          | TrackSlctPage
          | KBindsPage
          deriving (Bounded, Enum, Eq, Ord)
instance Show Page where
    show RacePage = "Start"
    show TrackSlctPage = "Track Select"
    show KBindsPage = "Key Bindings"

data LocState = LocState { _actPage :: Maybe Page
                         , _kBinds :: [(PlayerSignal, [Binding])]
                         , _kBindsAdded
                         , _trackBodyPassed :: Bool
                         , _slctMenuItemIx :: Int
                         , _core :: CoreState
                         }


makeFieldsNoPrefix ''LocState


data Console
instance Driver Console where
    run _ parser coreState = do
        conf <- ask
        initGlobState <- initialiseFlowState
        (evChan, globStateRef) <- liftIO $ do
            evChan <- newBChan 10
            globStateRef <- newIORef initGlobState
            let currFlow = runReader flow1 conf
                dispatch' = writeBChan evChan
            runFlow globStateRef dispatch' currFlow
            return (evChan, globStateRef)
        app' <- app globStateRef parser
        kBinds' <- liftIO getKBinds
        let bldVty = Vty.mkVty Vty.defaultConfig
            initLocState' = initLocState coreState kBinds'
        liftIO $ do
            initVty <- bldVty
            void $ customMain initVty bldVty (Just evChan) app' initLocState'

app :: Parser p
    => IORef FlowState
    -> Proxy p
    -> ReaderT Configuration IO (App LocState Action ())
app globStateRef parser = do
    draw' <- draw
    hndlEv' <- hndlEv globStateRef parser
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
    let f (LocState (Just RacePage)
                    _
                    _
                    _
                    _
                    (CoreState (CharacterState hp _ superpower')
                               score'
                               trackState
                    )
          )
            =
            let trackPiece = trackState ^. rows . to (take trackPieceCap)
            in
                [ center $ hCenter (str $ "SCORE: " ++ show score')
                           <=> hCenter (str . show
                                            . RndredTrackLines
                                            $ trackPiece
                                       )
                           <=> case superpower' of
                                   Just (BerserkerSuperpower remRows)
                                       ->
                                       hCenter (str $ "ARMAR: " ++ show remRows)
                                   Nothing
                                       ->
                                       emptyWidget
                           <=> hCenter (str $ "HP: " ++ show hp)
                ]
        f (LocState (Just KBindsPage) kBinds' kBindsAdded' _ slctMenuItemIx' _)
            = case sig' of
                  StrafeLeft
                      ->
                      [ hCenter (str $ show KBindsPage)
                        <=> center (kBindWid StrafeLeft True kBindsAdded'
                                    <=> kBindWid StrafeRight False False
                                    <=> kBindWid SwingLeft False False
                                    <=> kBindWid SwingRight False False
                                   )
                      ]
                  StrafeRight
                      ->
                      [ hCenter (str $ show KBindsPage)
                        <=> center (kBindWid StrafeLeft False False
                                    <=> kBindWid StrafeRight True kBindsAdded'
                                    <=> kBindWid SwingLeft False False
                                    <=> kBindWid SwingRight False False
                                   )
                      ]
                  SwingLeft
                      ->
                      [ hCenter (str $ show KBindsPage)
                        <=> center (kBindWid StrafeLeft False False
                                    <=> kBindWid StrafeRight False False
                                    <=> kBindWid SwingLeft True kBindsAdded'
                                    <=> kBindWid SwingRight False False
                                   )
                      ]
                  SwingRight
                      ->
                      [ hCenter (str $ show KBindsPage)
                        <=> center (kBindWid StrafeLeft False False
                                    <=> kBindWid StrafeRight False False
                                    <=> kBindWid SwingLeft False False
                                    <=> kBindWid SwingRight True kBindsAdded'
                                   )
                      ]
                  _
                      ->
                      []
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
            sig' = toEnum slctMenuItemIx' :: PlayerSignal
        f (LocState (Just TrackSlctPage)
           _
           _
           _
           slctMenuItemIx'
           (CoreState _ _ (TrackState _ _ name'))
          )
            = [ center . foldl (<=>) emptyWidget
                       . map str
                       . (& imap (\ix' trackName'
                                  ->
                                  if | ix' == slctMenuItemIx'
                                     && trackName' == name'
                                     -> "* " ++ trackName' ++ " *"
                                     | ix' == slctMenuItemIx'
                                     -> "* " ++ trackName'
                                     | trackName' == name'
                                     -> "  " ++ trackName' ++ " *"
                                     | otherwise
                                     -> "  " ++ trackName'
                                 )
                         )
                       $ Map.keys tracks'
              ]
        f (LocState Nothing _ _ _ slctMenuItemIx' _)
            =
            case toEnum @Page slctMenuItemIx' of
                RacePage -> slctRacePage
                TrackSlctPage -> slctTrackSlctPage
                KBindsPage -> slctKBindsPage
          where
            slctKBindsPage
                =
                [ hCenter (str "Main")
                  <=> center (str ("  " ++ show RacePage)
                  <=> str ("  " ++ show TrackSlctPage)
                  <=> str ("* " ++ show KBindsPage))
                ]
            slctRacePage
                =
                [ hCenter (str "Main")
                  <=> center (str ("* " ++ show RacePage)
                              <=> str ("  " ++ show TrackSlctPage)
                              <=> str ("  " ++ show KBindsPage)
                             )
                ]
            slctTrackSlctPage
                =
                [ hCenter (str "Main")
                  <=> center (str ("  " ++ show RacePage)
                              <=> str ("* " ++ show TrackSlctPage)
                              <=> str ("  " ++ show KBindsPage)
                             )
                ]
    return f

hndlEv :: Parser p
       => IORef FlowState
       -> Proxy p
       -> ReaderT Configuration
                  IO
                  (BrickEvent () Action -> EventM () LocState ())
hndlEv globStateRef parser = do
    trackPieceCap <- asks (^. preferences
                           . trackPieceCapacity
                           . to fromIntegral
                          )
    conf <- ask
    let interpretFrom' = configureFrom conf
        interpret'' = configure conf
    return $ \ev -> do
        currTrackName <- use (core . track . name)
        let currTrack = tracks' Map.! currTrackName
        case ev of
            AppEvent (Signal (FlowSignal Progress)) -> do
                prevCharRow <- use (core
                                    . character
                                    . position
                                    . unPosition
                                    . _1
                                   )
                let sig = FlowSignal Progress
                core %= (`runReader` conf) . (reflect sig)
                nextCharRow <- use (core
                                    . character
                                    . position
                                    . unPosition
                                    . _1
                                   )
                (charRow, charCol) <- bimap (fromIntegral @Natural @Int)
                                            (fromIntegral @Natural @Int)
                                   <$> use (core
                                            . character
                                            . position
                                            . unPosition
                                           )
                cellAheadChar <- gets (^? core
                                       . track
                                       . rows
                                       . ix (charRow + 1)
                                       . ix charCol
                                      )
                charHP <- use (core . character . hitPoints)
                let charProgressed = prevCharRow /= nextCharRow
                    obstAheadChar = isJust $ isObstacle <$> cellAheadChar
                    charDead = charHP == 0
                    charHit = not charProgressed && obstAheadChar
                liftIO $ do
                    modifyIORef globStateRef (& characterStuck .~ charHit)
                when charDead $ do
                    actPage .= Nothing
                    liftIO $ do
                        modifyIORef globStateRef $ (& started .~ False)
                                                   . (& paused .~ False)
                                                   . (& characterStuck .~ False)
                        savExists <- doesFileExist savFileName
                        when savExists $ removeFile savFileName
                when charHit . liftIO $ do
                    modifyIORef globStateRef (& characterHitsCount %~ (+ 1))
            AppEvent FeedNextTrackPiece -> do
                core . track . rows %= drop (trackPieceCap - 1)
                core . track
                     . rows
                     %= (\(part, rest)
                         ->
                         part ++ if null part then [] else [last part] ++ rest
                        )
                        . splitAt (trackPieceCap - 1)
            AppEvent (FeedNextTrackCycle gen) -> do
                prevTrackState <- use (core . track)
                let initTrackState = prevTrackState & rows %~ (pure . last)
                    currTrackCyc = getCycle currTrack
                    contTrackState = interpretFrom' initTrackState
                                                    currTrackCyc
                                                    gen
                                   & rows %~ reverse . init
                core . track .= contTrackState
                core . track
                     . rows
                     %= (\(part, rest) -> part ++ [last part] ++ rest)
                        . splitAt (trackPieceCap - 1)
                liftIO $ do
                    modifyIORef globStateRef
                                (& trackRowsCount
                                 .~ contTrackState
                                 ^. rows
                                 . to length
                                )
            AppEvent ReturnCharacter -> do
                core . character . position %= backtrack
            AppEvent (Refresh gen) -> do
                let currTrackState = interpret'' currTrack gen & rows %~ reverse
                    initExtState = runReader (initialiseCoreState currTrackState)
                                             conf
                trackBodyPassed' <- use trackBodyPassed
                trackBodyPassed .= True
                when trackBodyPassed' $ core .= initExtState
                core . track . name .= currTrackName
                core . track
                     . rows
                     %= (\(part, rest)
                         ->
                         part ++ if null part then [] else [last part] ++ rest
                        )
                        . splitAt (trackPieceCap - 1)
                liftIO $ do
                    modifyIORef globStateRef
                                (& trackRowsCount
                                 .~ currTrackState
                                 ^. rows
                                 . to length
                                )
                    savExists <- doesFileExist savFileName
                    when savExists $ removeFile savFileName
            VtyEvent (Vty.EvKey k mods) -> do
                if | k == (Vty.KChar 'q') -> do
                    kBinds' <- (& each
                                . _2
                                %~ Text.intercalate "," . map ppBinding
                               )
                            <$> use kBinds
                    coreState <- use core
                    liftIO $ do
                        FlowState {..} <- readIORef globStateRef
                        maybe (return ()) killThread _sessionThreadId
                        kBindsExist <- doesFileExist kBindsSavFileName
                        when kBindsExist $ removeFile kBindsSavFileName
                        writeFile kBindsSavFileName $ show kBinds'
                        let (Position charPos) = coreState
                                               ^. character
                                               . position
                            charRowIx = fromIntegral $ fst charPos
                            sav = ByteString.unpack
                                . serialiseCoreState parser
                                $ coreState & track . rows
                                                    %~ drop (charRowIx - 1)
                                            & character . position
                                                        %~ backtrack

                        writeFile savFileName sav
                    halt
                   | k == Vty.KBS -> do
                    actPage' <- use actPage
                    case actPage' of
                        Just KBindsPage -> do
                            kBinds' <- use kBinds
                            slctMenuItemIx' <- use slctMenuItemIx
                            let sig = toEnum slctMenuItemIx' :: PlayerSignal
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
                            slctPage' <- toEnum @Page <$> use slctMenuItemIx
                            actPage .= Just slctPage'
                            when (slctPage' == RacePage) $ do
                                liftIO $ do
                                    modifyIORef globStateRef
                                                $ (& paused .~ False)
                                                  . (& started .~ False)
                            slctMenuItemIx .= 0
                        Just TrackSlctPage -> do
                            trackIx <- use slctMenuItemIx
                            let trackName' = Map.keys tracks' NEList.!! trackIx
                            core . track . name .= trackName'
                        _ -> do
                            return ()
                   | k == Vty.KEsc -> do
                    actPage' <- use actPage
                    if actPage' == Nothing
                    then do
                        slctPage' <- toEnum @Page <$> use slctMenuItemIx
                        actPage .= Just slctPage'
                        liftIO $ do
                            modifyIORef globStateRef
                                        $ ((& paused .~ False)
                                           . (& started .~ False)
                                          )
                    else do
                        actPage .= Nothing
                   | k `elem` [Vty.KChar 'w', Vty.KUp] -> do
                    slctMenuItemIx' <- use slctMenuItemIx
                    actPage' <- use actPage
                    case actPage' of
                        Just KBindsPage -> do
                            let playerSig = toEnum @PlayerSignal slctMenuItemIx'
                            if (playerSig > minBound)
                            then do
                                slctMenuItemIx %= pred
                            else do
                                slctMenuItemIx .= fromEnum @PlayerSignal
                                                           (maxBound @PlayerSignal)
                        Just RacePage -> do
                            return ()
                        Just TrackSlctPage -> do
                            if (slctMenuItemIx' > 0)
                            then do
                               slctMenuItemIx %= pred
                            else do
                               let tracksCnt = length tracks
                               slctMenuItemIx .= tracksCnt
                        Nothing -> do
                            let page = toEnum @Page slctMenuItemIx'
                            if (page > minBound)
                            then do
                                slctMenuItemIx %= pred
                            else do
                                slctMenuItemIx .= fromEnum @Page
                                                           (maxBound @Page)
                   | k `elem` [Vty.KChar 's', Vty.KDown] -> do
                    slctMenuItemIx' <- use slctMenuItemIx
                    actPage' <- use actPage
                    case actPage' of
                        Just KBindsPage -> do
                            let sig = toEnum @PlayerSignal slctMenuItemIx'
                            if (sig < maxBound)
                            then slctMenuItemIx %= succ
                            else slctMenuItemIx .= 0
                        Just RacePage -> do
                            return ()
                        Just TrackSlctPage -> do
                            let slctTrack = slctMenuItemIx'
                                tracksCnt = length tracks
                            if (slctTrack < tracksCnt)
                            then slctMenuItemIx %= succ
                            else slctMenuItemIx .= 0
                        Nothing -> do
                            let page = toEnum @Page slctMenuItemIx'
                            if (page < maxBound)
                            then slctMenuItemIx %= succ
                            else slctMenuItemIx .= 0
                   | otherwise -> do
                    let kBind = binding k mods
                    kBinds' <- use kBinds
                    kBindsAdded' <- use kBindsAdded
                    if kBindsAdded'
                    then do
                        pageItemIx' <- use slctMenuItemIx
                        let sig = toEnum pageItemIx' :: PlayerSignal
                            kBindIx = List.findIndex ((== sig) . fst) kBinds'
                        case kBindIx of
                            Just kBindIx' -> do
                                kBinds %= (^.. traversed
                                           . to (& _2 %~ filter (/= kBind))
                                          )
                                kBinds . ix kBindIx' . _2 %= (++ [kBind])
                            Nothing -> do
                                return ()
                    else case List.findIndex (elem kBind . snd) kBinds' of
                        Just sigIx -> do
                            let playerSig = fst $ kBinds' !! sigIx
                                sig = PlayerSignal playerSig
                            FlowState {..} <- liftIO $ readIORef globStateRef
                            when (not _paused) $ do
                                core %= (`runReader` conf) . (reflect sig)
                                cellAheadChar <- getCellAheadChar
                                charHP <- use (core . character . hitPoints)
                                let charDead = charHP == 0
                                when charDead $ do
                                    actPage .= Nothing
                                    liftIO $ do
                                        savExists <- doesFileExist savFileName
                                        when savExists $ removeFile savFileName
                                liftIO $ do
                                    let obstAheadChar = cellAheadChar
                                                      == Just Obstacle
                                    modifyIORef globStateRef
                                                (& characterStuck
                                                 .~ obstAheadChar
                                                )
                        Nothing -> do
                            return ()
            _ -> do
                 return ()

initLocState :: CoreState -> [(PlayerSignal, [Binding])] -> LocState
initLocState coreState kBinds' = LocState Nothing kBinds' False False 0 coreState

defKBinds :: [(PlayerSignal, [Binding])]
defKBinds
   =
   [ (StrafeLeft, [bind 'h'])
   , (StrafeRight, [bind 'l'])
   , (SwingLeft, [bind 'j'])
   , (SwingRight, [bind 'k'])
   ]

getKBinds :: IO [(PlayerSignal, [Binding])]
getKBinds = do
    kBindsExist <- doesFileExist kBindsSavFileName
    if kBindsExist
    then do
        readFile kBindsSavFileName <&> (\case
                                            Right kBinds' -> kBinds'
                                            Left _ -> defKBinds
                                       )
                                       . readKBinds
    else return defKBinds
  where
    readKBinds = readEither @[(PlayerSignal, Text.Text)]
               >=> (& sequence)
                   . (& each %~ sequence)
                   . (& each . _2
                             %~ ((\case
                                      BindingList kBinds' -> kBinds'
                                      Unbound -> []
                                 ) <$>
                                )
                                . parseBindingList
                     )

kBindsSavFileName :: String
kBindsSavFileName = ".curr-k-binds.sav"

getCellAheadChar :: EventM () LocState (Maybe Cell)
getCellAheadChar = do
    charPos <- use (core . character . position)
    let charRowIx = charPos ^. unPosition . _1 . to fromIntegral
        charColIx = charPos ^. unPosition . _2 . to fromIntegral
    gets (^? core . track . rows . ix (charRowIx + 1) . ix charColIx)

salvageChar :: EventM () LocState ()
salvageChar = do
    row <- use (core . track . rows . _head)
    charColIx <- fromIntegral <$> use (core
                                       . character
                                       . position
                                       . unPosition
                                       . _2
                                      )
    let passCellIxs = List.findIndices (== TrailPart) row
        nearestPassCellIx = foldr (\passCellIx nearestPassCellIx'
                                   ->
                                   if abs (passCellIx - charColIx)
                                      < nearestPassCellIx'
                                   then passCellIx
                                   else nearestPassCellIx'
                                  )
                                  charColIx
                                  passCellIxs
    core . character
         . position
         . unPosition
         . _2
         .= fromIntegral nearestPassCellIx

savFileName :: String
savFileName = ".curr-rac-prog.sav"

runFlow :: IORef FlowState -> (Action -> IO ()) -> Flow r n -> IO ()
runFlow _ _ (Pure _) = pure ()
runFlow globStateRef sink (Free (Dispatch act next')) = do
    sink act
    runFlow globStateRef sink $ next' ()
runFlow globStateRef sink (Free (Forever flow next')) = do
    _ <- forever $ runFlow globStateRef sink flow
    runFlow globStateRef sink $ next' ()
runFlow globStateRef sink (Free (Fork flow next')) = do
    threadId <- forkIO $ runFlow globStateRef sink flow
    runFlow globStateRef sink $ next' threadId
runFlow globStateRef sink (Free (MakeGenerator next')) = do
    gen <- newStdGen 
    runFlow globStateRef sink $ next' gen
runFlow globStateRef sink (Free (ModifyState f next')) = do
    modifyIORef globStateRef f
    runFlow globStateRef sink $ next' ()
runFlow globStateRef sink (Free (KillThread threadId next')) = do
    killThread threadId 
    runFlow globStateRef sink $ next' ()
runFlow globStateRef sink (Free (ReadState next')) = do
    globState <- readIORef globStateRef
    runFlow globStateRef sink $ next' globState
runFlow globStateRef sink (Free (Replicate n flow next')) = do
    replicateM_ n (runFlow globStateRef sink flow)
    runFlow globStateRef sink $ next' ()
runFlow globStateRef sink (Free (Until suspFlow f flow next')) = do
    globState <- readIORef globStateRef
    if not $ f globState
    then do
        runFlow globStateRef sink suspFlow
        runFlow globStateRef sink $ until' suspFlow f flow
        runFlow globStateRef sink $ next' ()
    else do
        runFlow globStateRef sink flow
        runFlow globStateRef sink $ next' ()
runFlow globStateRef sink (Free (Wait t next')) = do
    threadDelay t
    runFlow globStateRef sink $ next' ()
runFlow globStateRef sink (Free (WriteState globState next')) = do
    writeIORef globStateRef globState
    runFlow globStateRef sink $ next' ()
#endif
