{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}


module Driver.Driver.Www.View.Rac where


import Core.Core
import Core.Track.Character.Character

import Driver.Www.Comm

import Miso
import Numeric.Natural
import qualified Miso.String as Ms


rac :: Mdl -> View Act
rac Mdl {_core = CoreState {_character = CharacterState {_hitPoints}, _score}}
    =
    main_ [class_ "rac-page"]
          [ section_ [class_ "score"] [text score']
          , section_ [class_ "shields rac-page__shields"]
                     $ replicate hitPoints'
                                 (span_ [class_ "shield rac-page__shield"] [])
          ]
  where
    hitPoints' = fromIntegral @Natural @Int _hitPoints
    score' = Ms.ms $ show _score
