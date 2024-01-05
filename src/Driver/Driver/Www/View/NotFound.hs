{-# LANGUAGE OverloadedStrings #-}


module Driver.Driver.Www.View.NotFound where


import Driver.Www.Comm

import Miso


notFound :: View Act
notFound
    =
    div_ [] [ text "PAGE NOT FOUND"
            , button_ [onClick goToHome] [ text "MAIN MENU" ]
            ]
