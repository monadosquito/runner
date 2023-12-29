{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}


import qualified Core.Port.Driver as DriverPort

#if defined(CONS)
import qualified Driver.Driver.Brick as Driver
#elif defined(WWW)
import qualified Driver.Driver.Www.Www as Driver
#endif
import Driver.Parser.Aeson

import Control.Monad.Reader
import Data.Proxy

import Core.Configuration.Configuration
import Core.Port.Environment

#if defined(CONS)
import qualified Driver.Environment.Console as Env
#elif defined(WWW)
import qualified Driver.Environment.Www as Env
#endif

#if defined(WWW) && !defined(__GHCJS__)
import Language.Javascript.JSaddle
import qualified Language.Javascript.JSaddle.Warp as Jsaddle
#endif


#if defined(CONS)
type Driver = Driver.Console
type Env = Env.Console
#elif defined(WWW)
type Driver = Driver.Www
type Env = Env.Www
#endif


main :: IO ()
main = runApp $ do
    prefs <- getPreferences (Proxy @Env) $ Proxy @Aeson
    let conf = Configuration prefs defaultOptions
    coreState <- runReaderT (getCoreState (Proxy @Env) $ Proxy @Aeson) conf
    liftIO $ runReaderT (DriverPort.run (Proxy @Driver)
                                        (Proxy @Aeson)
                                        coreState
                        )
                        conf

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
