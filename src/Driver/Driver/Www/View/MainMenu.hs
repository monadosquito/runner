{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}


module Driver.Driver.Www.View.MainMenu where


import Driver.Www.Comm

import Miso


mainMenu :: Mdl -> View Act
mainMenu _
    =
    main_ [class_ "page"]
          [ h3_ [class_ "page__cap"] [text "MAIN MENU"]
          , ul_ [class_ "menu page__menu"]
                [ button_ [class_ "menu__btn", onClick goToRac] [text "START"]
                , button_ [class_ "menu__btn", onClick goToPrefs]
                          [text "PREFERENCES"]
                , button_ [class_ "menu__btn", onClick goToKBinds]
                          [text "KEY BINDINGS"]
                ]
          ]
