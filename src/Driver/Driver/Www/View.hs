{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}


module Driver.Driver.Www.View where


import Driver.Driver.Www.View.KBinds
import Driver.Driver.Www.View.MainMenu
import Driver.Driver.Www.View.NotFound
import Driver.Driver.Www.View.Prefs
import Driver.Driver.Www.View.Rac
import Driver.Www.Comm

import Data.Proxy
import Miso
import Miso.String
import Servant.API


threeLink :: MisoString
threeLink = "https://ajax.googleapis.com/ajax/libs/threejs/r84/three.min.js"

mainView :: Mdl -> View Act
mainView mdl
    =
    div_ [] [ script_ [src_ threeLink] ""
            , canvas_ [class_ "canv canv_hidden", id_ "canv"] []
            , ui
            ]
  where
    ui = either (const notFound) id $ runRoute (Proxy @Api) handlers _uri mdl
    handlers = mainMenu :<|> kBinds' :<|> prefs' :<|> rac
