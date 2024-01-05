{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}


module Driver.Driver.Www.View.Prefs where


import Core.Configuration.Configuration
import Core.Track.Track

import Core.Script.Track

import Driver.Www.Comm

import Control.Lens
import Miso
import qualified Miso.String as Ms
import qualified Data.Map as Map


prefs' :: Mdl -> View Act
prefs' Mdl {_core, _prefs = Preferences {..}}
    =
    main_ [class_ "page"]
          [ h3_ [class_ "page__cap"] [text "PREFERENCES"]
          , ul_ [class_ "menu page__menu"]
                [ div_ [class_ "input"]
                       [ label_ [for_ "trackName"] [text "SELECTED TRACK"]
                       , select_ [ class_ "input__field"
                                 , id_ "trackName"
                                 , onChange $ MisoAct . SelectTrack
                                 ]
                                 $ map mkTrack trackNames
                       ]
                , div_ [class_ "input"]
                       [ label_ [for_ "trackPieceCap"]
                                [text "TRACK PIECE CAPACITY"]
                       , input_ [ class_ "input__field"
                                , id_ "trackPieceCap"
                                , onInput $ MisoAct . AlterTrackPieceCap
                                , type_ "number"
                                , value_ trackPieceCap
                                ]
                       ]
                ]
          , button_ [class_ "page__btn", onClick $ MisoAct RestDefKBinds]
                    [text "RESTORE DEFAULTS"]
          ]
  where
    mkTrack (Ms.ms -> trackName')
        | trackName' == selectedTrackName
        = option_ [selected_ True] [text trackName']
        | otherwise
        = option_ [] [text trackName']
    selectedTrackName = _core ^. track . name . to Ms.ms
    trackNames = Map.keys tracks'
    trackPieceCap = Ms.ms $ show _trackPieceCapacity
