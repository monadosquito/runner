{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}


module Driver.Driver.Www.View.KBinds where


import Core.Signal.Signal

import Driver.Www.Comm

import Miso
import qualified Miso.String as Ms
import qualified Data.Map as Map


mkKBind :: Maybe PlayerSignal -> (PlayerSignal, Int) -> View Act
mkKBind boundPlayerSig' (sig', kCode')
    =
    li_ [ class_ $ "input" <> if sigBound then " input_bound" else ""
        , onClick $ MisoAct (WaitNewK sig')
        ]
        [ span_ [] [text sig]
        , span_ [ class_ $ "input__key-code"
                         <> if sigBound
                            then " input__key-code_hidden"
                            else ""
                ]
                [text kCode]
        ]
  where
    kCode = Ms.ms $ show kCode'
    sig = Ms.ms $ show sig'
    sigBound = Just sig' == boundPlayerSig'

kBinds' :: Mdl -> View Act
kBinds' Mdl {_boundPlayerSig, _kBinds}
    =
    main_ [class_ "page"]
          [ h3_ [class_ "page__cap"] []
          , ul_ [class_ "menu page__menu"]
                $ map (mkKBind _boundPlayerSig) (Map.toList _kBinds)
          , button_ [class_ "page__btn", onClick $ MisoAct RestDefKBinds]
                    [text "RESTORE DEFAULTS"]
          ]
