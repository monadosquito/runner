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
import qualified Core.Track.Character.Character as Char
import Core.Track.Configuration.Configuration
import qualified Core.Track.Track as Track

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

import qualified Core.Port.Parser as Parser

import Data.Proxy
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.List.NonEmpty as NEList


data Page = RacePage
          | TrackSlctPage
          | KBindsPage
          deriving (Bounded, Enum, Eq, Ord)
instance Show Page where
    show RacePage = "Start"
    show TrackSlctPage = "Track Select"
    show KBindsPage = "Key Bindings"

data GlobState = GlobState { _currTrackCycPiecesCnt
                           , _currTrackCycRemRowsCnt
                           , _currTrackPiecesCnt
                           , _currTrackRemRowsCnt
                           , _currTrackPieceCharHitsCnt :: Int
                           , _flowThreadId
                           , _sessThreadId :: Maybe ThreadId
                           , _paused
                           , _started
                           , _charStrafed
                           , _charStuck :: Bool
                           }

data LocState = LocState { _actPage :: Maybe Page
                         , _kBinds :: [(Signal, [Binding])]
                         , _kBindsAdded :: Bool
                         , _slctMenuItemIx :: Int
                         , _ext :: Parser.ExternalState
                         }


makeFieldsNoPrefix ''LocState
makeFieldsNoPrefix ''GlobState


data Brick
instance Driver Brick where
    run _ parser = do
        currTrackName <- asks (^. preferences . trackName)
        conf <- ask
        trackPieceCap <- asks (^. preferences
                               . trackPieceCapacity
                               . to fromIntegral
                              )
        charProgS <- asks (^. options . characterProgressSpeed)
        (currTrackState, evChan, globStateRef) <- liftIO $ do
            gen <- newStdGen
            let currTrack = tracks' Map.! currTrackName
                currTrackCyc = Track.getCycle currTrack
                currTrackInf = is _Free currTrackCyc
                interpret'' = configure conf
                currTrackState = interpret'' currTrack gen
                trackRowTime = round $ 1 / charProgS * 1000000
            evChan <- newBChan 10
            globStateRef <- newIORef initGlobState
            let feedTrackRow = do
                    GlobState {..} <- readIORef globStateRef
                    threadDelay trackRowTime
                    let freezed = _paused || _charStuck
                    if not freezed
                    then writeBChan evChan FeedTrackRow
                    else feedTrackRow
                feedHitTrackRows = do
                    GlobState {..} <- readIORef globStateRef
                    when (_currTrackPieceCharHitsCnt > 0) $ do
                        modifyIORef globStateRef
                                    (& currTrackPieceCharHitsCnt .~ 0)
                        replicateM_ _currTrackPieceCharHitsCnt feedTrackRow
                        feedHitTrackRows
                feedTrackRows initTrackRowsCnt = do
                    modifyIORef globStateRef
                                (& currTrackPieceCharHitsCnt
                                 .~ initTrackRowsCnt
                                )
                    feedHitTrackRows
                start = do
                    GlobState {..} <- readIORef globStateRef
                    replicateM_ _currTrackPiecesCnt $ do
                        feedTrackRows trackPieceCap
                        writeBChan evChan FeedTrackPiece
                        writeBChan evChan ReturnChar
                    feedTrackRows _currTrackRemRowsCnt
                    threadDelay 100000
                    if currTrackInf
                    then forever $ do
                        writeBChan evChan FeedTrackCyc
                        writeBChan evChan ReturnChar
                        threadDelay 100000
                        GlobState {..} <- readIORef globStateRef
                        replicateM_ _currTrackCycPiecesCnt $ do
                            feedTrackRows trackPieceCap
                            writeBChan evChan FeedTrackPiece
                            writeBChan evChan ReturnChar
                        feedTrackRows _currTrackCycRemRowsCnt
                        threadDelay 100000
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
        app' <- app globStateRef evChan parser
        kBinds' <- liftIO getKBinds
        extState <- getExtState parser currTrackState
        let bldVty = Vty.mkVty Vty.defaultConfig
            currTrackRowsCnt = extState ^. Parser.track . Track.rows . to length
            currTrackPiecesCnt' = currTrackRowsCnt `div` trackPieceCap
            currTrackRemRowsCnt' = currTrackRowsCnt
                                 `mod` trackPieceCap
                                 + (currTrackPiecesCnt' - 1)
            initLocState' = initLocState extState kBinds'
        liftIO $ do
            modifyIORef globStateRef
                        $ (& currTrackPiecesCnt .~ currTrackPiecesCnt')
                          . (& currTrackRemRowsCnt .~ currTrackRemRowsCnt')
            initVty <- bldVty
            void $ customMain initVty bldVty (Just evChan) app' initLocState'

data FlowEvent = FeedTrackRow
               | FeedTrackPiece
               | FeedTrackCyc
               | Fin
               | ReturnChar
               | Start


app :: Parser.Parser p
    => IORef GlobState
    -> BChan FlowEvent
    -> Proxy p
    -> ReaderT Configuration IO (App LocState FlowEvent ())
app globStateRef evChan parser = do
    draw' <- draw
    hndlEv' <- hndlEv globStateRef evChan parser
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
                    (Parser.ExternalState charState@(Char.State hp _)
                                          score'
                                          trackState
                    )
          )
            =
            let trackPiece = trackState ^. Track.rows . to (take trackPieceCap)
            in
                [ center $ hCenter (str $ "SCORE: " ++ show score')
                           <=> hCenter (str . show
                                            . RndredTrackLines
                                            $ Char.reflect charState trackPiece
                                       )
                           <=> hCenter (str $ "HP: " ++ show hp)
                ]
        f (LocState (Just KBindsPage) kBinds' kBindsAdded' slctMenuItemIx' _)
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
        f (LocState (Just TrackSlctPage)
           _
           _
           slctMenuItemIx'
           (Parser.ExternalState _ _ (Track.State _ _ name))
          )
            = [ center . foldl (<=>) emptyWidget
                       . map str
                       . (& imap (\ix' trackName'
                                  ->
                                  if | ix' == slctMenuItemIx'
                                     && trackName' == name
                                     -> "* " ++ trackName' ++ " *"
                                     | ix' == slctMenuItemIx'
                                     -> "* " ++ trackName'
                                     | trackName' == name
                                     -> "  " ++ trackName' ++ " *"
                                     | otherwise
                                     -> "  " ++ trackName'
                                 )
                         )
                       . NEList.toList
                       $ Map.keys tracks'
              ]
        f (LocState Nothing _ _ slctMenuItemIx' _)
            =
            case toEnum slctMenuItemIx' of
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

hndlEv :: Parser.Parser p
       => IORef GlobState
       -> BChan FlowEvent
       -> Proxy p
       -> ReaderT Configuration
                  IO
                  (BrickEvent () FlowEvent -> EventM () LocState ())
hndlEv globStateRef evChan parser = do
    opts <- asks _options
    trackPieceCap <- asks (^. preferences
                           . trackPieceCapacity
                           . to fromIntegral
                          )
    conf <- ask
    let interpretFrom' = configureFrom conf
        interpret'' = configure conf
    return $ \ev -> do
        currTrackName <- use (ext . Parser.track . Track.name)
        let currTrack = tracks' Map.! currTrackName
        case ev of
            AppEvent FeedTrackRow -> do
                prevCharPos <- use (ext . Parser.character . Char.position)
                trackRows <- use (ext . Parser.track . Track.rows)
                ext . Parser.character
                    %= Char.obstruct (progress prevCharPos) trackRows
                charHP <- use (ext . Parser.character . Char.hitPoints)
                cellAheadChar <- getCellAheadChar
                nextCharPos <- use (ext . Parser.character . Char.position)
                let obstAheadChar = cellAheadChar == Just Track.Obstacle
                    charDead = charHP == 0
                    charMoved = prevCharPos /= nextCharPos
                if (obstAheadChar && not charMoved)
                then liftIO $ do
                    modifyIORef globStateRef
                                (& currTrackPieceCharHitsCnt %~ (+ 1))
                else do
                    ext . Parser.score %= (+ 1)
                liftIO $ do
                    modifyIORef globStateRef
                                $ (& charStrafed .~ False)
                                  . (& charStuck .~ obstAheadChar)
                    when charDead $ do
                        savExists <- doesFileExist savFileName
                        when savExists $ removeFile savFileName
                        writeBChan evChan Fin
                        writeBChan evChan Start
            AppEvent FeedTrackPiece -> do
                ext . Parser.track . Track.rows %= drop (trackPieceCap - 1)
                liftIO $ do
                    modifyIORef globStateRef (& currTrackPieceCharHitsCnt .~ 0)
            AppEvent FeedTrackCyc -> do
                gen <- newStdGen
                prevTrackState <- use (ext . Parser.track)
                let initTrackState = prevTrackState
                                   & Track.rows
                                   %~ (pure . last)
                    currTrackCyc = Track.getCycle currTrack
                    contTrackState = interpretFrom' initTrackState
                                                    currTrackCyc
                                                    gen
                    currTrackCycRowsCnt = contTrackState
                                        ^. Track.rows
                                        . to length
                    currTrackCycPiecesCnt' = currTrackCycRowsCnt
                                           `div` trackPieceCap
                    currTrackCycRemRowsCnt' = currTrackCycRowsCnt
                                            `mod` trackPieceCap
                                            + (currTrackCycPiecesCnt' - 1)
                ext . Parser.track .= contTrackState
                ext . Parser.track . Track.rows %= reverse
                salvageChar
                liftIO $ do
                    modifyIORef globStateRef
                                $ (& currTrackCycPiecesCnt
                                   .~ currTrackCycPiecesCnt'
                                  )
                                  . (& currTrackCycRemRowsCnt
                                     .~ currTrackCycRemRowsCnt'
                                    )
                                  . (charStuck .~ False)
            AppEvent Fin -> do
                actPage .= Nothing
            AppEvent ReturnChar -> do
                ext . Parser.character . Char.position %= backtrack
            AppEvent Start -> do
                gen <- newStdGen
                let currTrackState = interpret'' currTrack gen
                    currTrackRowsCnt = currTrackState ^. Track.rows . to length
                    currTrackPiecesCnt' = currTrackRowsCnt
                                        `div` trackPieceCap
                    currTrackRemRowsCnt' = currTrackRowsCnt
                                         `mod` trackPieceCap
                                         + (currTrackPiecesCnt' - 1)
                ext . Parser.character .= runReader Char.revive opts
                oldCurrTrackName <- use (ext . Parser.track . Track.name)
                ext . Parser.track .= currTrackState
                ext . Parser.track . Track.rows %= reverse
                ext . Parser.score .= 0
                ext . Parser.track . Track.name .= oldCurrTrackName
                liftIO $ do
                    modifyIORef globStateRef
                                $ (& charStuck .~ False)
                                  . (& currTrackPiecesCnt
                                     .~ currTrackPiecesCnt'
                                    )
                                  . (& currTrackRemRowsCnt
                                     .~ currTrackRemRowsCnt'
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
                       extState <- use ext
                       liftIO $ do
                           GlobState {..} <- readIORef globStateRef
                           maybe (return ()) killThread _flowThreadId
                           maybe (return ()) killThread _sessThreadId
                           kBindsExist <- doesFileExist kBindsSavFileName
                           when kBindsExist $ removeFile kBindsSavFileName
                           writeFile kBindsSavFileName $ show kBinds'
                           let (Position charPos) = extState
                                                  ^. Parser.character
                                                  . Char.position
                               charRowIx = fromIntegral $ fst charPos
                               sav = ByteString.unpack
                                   . Parser.serialiseExternalState parser
                                   $ extState & Parser.track . Track.rows
                                                             %~ drop (charRowIx
                                                                      - 1
                                                                     )
                                              & Parser.character . Char.position
                                                                 %~ backtrack

                           writeFile savFileName sav
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
                                       writeBChan evChan Start
                               slctMenuItemIx .= 0
                           Just TrackSlctPage -> do
                               trackIx <- use slctMenuItemIx
                               let trackName' = Map.keys tracks'
                                              NEList.!! trackIx
                               ext . Parser.track . Track.name .= trackName'
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
                           modifyIORef globStateRef
                                       $ (& paused .~ False)
                                         . (started .~ False)
                   | k `elem` [Vty.KChar 'w', Vty.KUp] -> do
                       slctMenuItemIx' <- use slctMenuItemIx
                       actPage' <- use actPage
                       case actPage' of
                            Just KBindsPage -> do
                                let sig = toEnum @Signal slctMenuItemIx'
                                when (sig > minBound) $ slctMenuItemIx %= pred
                            Just RacePage -> do
                                return ()
                            Just TrackSlctPage -> do
                                when (slctMenuItemIx' > 0) $ do
                                    slctMenuItemIx %= pred
                            Nothing -> do
                                let page = toEnum @Page slctMenuItemIx'
                                when (page > minBound) $ slctMenuItemIx %= pred
                   | k `elem` [Vty.KChar 's', Vty.KDown] -> do
                       slctMenuItemIx' <- use slctMenuItemIx
                       actPage' <- use actPage
                       case actPage' of
                            Just KBindsPage -> do
                                let sig = toEnum @Signal slctMenuItemIx'
                                when (sig < maxBound) $ do
                                    slctMenuItemIx %= succ
                            Just RacePage -> do
                                return ()
                            Just TrackSlctPage -> do
                                let slctTrack = slctMenuItemIx'
                                    tracksCnt = length tracks
                                when (slctTrack < tracksCnt) $ do
                                    slctMenuItemIx %= succ
                            Nothing -> do
                                let page = toEnum @Page slctMenuItemIx'
                                when (page < maxBound) $ do
                                    slctMenuItemIx %= succ
                   | otherwise -> do
                       let kBind = binding k mods
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
                                              . to (& _2 %~ filter (/= kBind))
                                             )
                                   kBinds . ix kBindIx' . _2 %= (++ [kBind])
                               Nothing -> do
                                   return ()
                       else case List.findIndex (elem kBind . snd) kBinds' of
                           Just sigIx -> do
                               let sig = fst $ kBinds' !! sigIx
                               GlobState {..} <- liftIO $ readIORef globStateRef
                               when (not _paused) $ do
                                   trackRows <- use (ext
                                                     . Parser.track
                                                     . Track.rows
                                                    )
                                   nextCharPos <- (`runReader` conf)
                                                  . strafe (signalToSide sig)
                                               <$> use (ext
                                                        . Parser.character
                                                        . Char.position
                                                       )
                                   ext . Parser.character
                                       %= Char.obstruct nextCharPos trackRows
                                   cellAheadChar <- getCellAheadChar
                                   charHP <- use (ext
                                                  . Parser.character
                                                  . Char.hitPoints
                                                 )
                                   liftIO $ do
                                       let charDead = charHP == 0
                                           obstAheadChar = cellAheadChar
                                                           == Just Track.Obstacle
                                       modifyIORef globStateRef
                                                   $ (& charStrafed .~ True)
                                                     . (& charStuck
                                                        .~ obstAheadChar
                                                       )
                                       when charDead $ do
                                           savExists <- doesFileExist savFileName
                                           when savExists $ removeFile savFileName
                                           writeBChan evChan Fin
                                           writeBChan evChan Start
                           Nothing -> do
                               return ()
            _ -> do
                 return ()

initGlobState :: GlobState
initGlobState = GlobState 0 0 0 0 0 Nothing Nothing True True False False

initLocState :: Parser.ExternalState -> [(Signal, [Binding])] -> LocState
initLocState extState kBinds' = LocState Nothing kBinds' False 0 extState

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

getCellAheadChar :: EventM () LocState (Maybe Track.Cell)
getCellAheadChar = do
    charPos <- use (ext . Parser.character . Char.position)
    let charRowIx = charPos ^. unPosition . _1 . to fromIntegral
        charColIx = charPos ^. unPosition . _2 . to fromIntegral
    gets (^? ext
          . Parser.track
          . Track.rows
          . ix (charRowIx + 1)
          . ix charColIx
         )

salvageChar :: EventM () LocState ()
salvageChar = do
    row <- use (ext . Parser.track . Track.rows . _head)
    charColIx <- fromIntegral <$> use (ext
                                       . Parser.character
                                       . Char.position
                                       . unPosition
                                       . _2
                                      )
    let passCellIxs = List.findIndices (== Track.TrailPart) row
        nearestPassCellIx = foldr (\passCellIx nearestPassCellIx'
                                   ->
                                   if abs (passCellIx - charColIx)
                                      < nearestPassCellIx'
                                   then passCellIx
                                   else nearestPassCellIx'
                                  )
                                  charColIx
                                  passCellIxs
    ext . Parser.character
        . Char.position
        . unPosition
        . _2
        .= fromIntegral nearestPassCellIx

getExtState :: Parser.Parser p
            => Proxy p
            -> Track.State
            -> ReaderT Configuration IO Parser.ExternalState
getExtState prsr trackState = do
    initExtState' <- initExtState trackState
    liftIO $ do
        savExists <- doesFileExist savFileName
        if savExists
        then do
            sav <- readFile savFileName
            case Parser.deserialiseExternalState prsr $ ByteString.pack sav of
                Just savExtState -> return savExtState
                Nothing -> return initExtState'
        else do
            return initExtState'

initExtState :: Track.State -> ReaderT Configuration IO Parser.ExternalState
initExtState trackState = do
    opts <- asks _options
    let initCharPos = runReader spawn opts
    charHP <- asks (^. options . characterHitPoints)
    return $ Parser.ExternalState (Char.State charHP initCharPos) 0 trackState

savFileName :: String
savFileName = ".prog.sav"
