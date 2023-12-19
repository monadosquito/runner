{-# LANGUAGE CPP #-}


module Driver.Driver.Www.Js (module Js) where


#ifdef __GHCJS__
import Driver.Driver.Www.Js.Ffi as Js
#else
import Driver.Driver.Www.Js.Jsaddle as Js
#endif
