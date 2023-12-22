{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}


import Core.Port.Driver

import qualified Driver.Driver.Www.Www as Driver
import Driver.Parser.Aeson

import Control.Monad.Reader
import Data.Proxy

import Core.Configuration.Configuration
import Core.Port.Environment

import qualified Driver.Environment.Www as Env


#if defined(WWW) && !defined(__GHCJS__)
import Language.Javascript.JSaddle
import qualified Language.Javascript.JSaddle.Warp as Jsaddle
#endif
main :: IO ()
main = runApp $ do
    prefs <- getPreferences (Proxy @Env.Www) $ Proxy @Aeson
    let conf = Configuration prefs defaultOptions
    liftIO $ runReaderT (run (Proxy @Driver.Www) $ Proxy @Aeson) conf

#if defined(CONS)
runApp :: IO () -> IO ()
runApp = id
#elif defined(WWW)
#ifdef __GHCJS__
runApp :: IO () -> IO ()
runApp = id
#else
runApp :: JSM () -> IO ()
runApp f = Jsaddle.debugOr 8000 (f >> syncPoint) Jsaddle.jsaddleApp
#endif
#endif
