{-# LANGUAGE OverloadedStrings #-}


module Driver.Driver.Www.View.Main where


import Driver.Www.Comm

import Miso
import Miso.String


threeLink :: MisoString
threeLink = "https://ajax.googleapis.com/ajax/libs/threejs/r84/three.min.js"

mainView :: Mdl -> View Act
mainView _ = div_ [] [script_ [src_ threeLink] ""]
