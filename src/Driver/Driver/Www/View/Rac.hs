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
rac Mdl {_core = CoreState { _character = CharacterState { _hitPoints
                                                         , _superpower
                                                         }
                           , _score
                           }
        }
    =
    main_ [class_ "rac-page"]
          [section_ [class_ "score"] [text score'], shields _superpower]
  where
    hitPoints' = fromIntegral @Natural @Int _hitPoints
    score' = Ms.ms $ show _score
    shield Nothing = span_ [class_ $ "shield" <> " rac-page__shield" ] []
    shield (Just (BerserkerSuperpower _))
        =
        span_ [class_ $ "shield shield_plated rac-page__shield" ] []
    shields mortality'
        =
        section_ [class_ "char-state"]
                 [ div_ [class_ "shields rac-page__shields"]
                        $ replicate hitPoints' (shield mortality')
                 , bersBonRemRowsCnt mortality'
                 ]
    bersBonRemRowsCnt Nothing = div_ [] []
    bersBonRemRowsCnt (Just (BerserkerSuperpower bonRemRowsCnt'))
        =
        div_ [class_ "bon-rem-rows-cnt"] [text bonRemRowsCnt]
      where
        bonRemRowsCnt = Ms.ms $ show bonRemRowsCnt'
