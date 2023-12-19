{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}


module Driver.Driver.Www.Js.Jsaddle where

#ifndef __GHCJS__
import Control.Monad
import Language.Javascript.JSaddle


add :: JSVal -> JSVal -> JSM ()
add parent child = void $ parent # ("add" :: String) $ child

clearChildren :: JSVal -> JSM ()
clearChildren obj' = do
    obj' ! ("children" :: String) <# ("length" :: String) $ (0 :: Int)

getObjByName :: JSVal -> JSVal -> JSM JSVal
getObjByName parent childName = do
    parent # ("getObjectByName" :: String) $ [childName]

newEdgesGeom :: JSVal -> JSM JSVal
newEdgesGeom geom = do
    three <- jsg ("THREE" :: String)
    new (three ! ("EdgesGeometry" :: String)) $ geom

newLineBasMat :: JSVal -> JSM JSVal
newLineBasMat col = do
    three <- jsg ("THREE" :: String)
    args <- obj
    setProp "color" col args
    jsArgs <- toJSVal args
    new (three ! ("LineBasicMaterial" :: String)) $ jsArgs

newLineSegs :: JSVal -> JSVal -> JSM JSVal
newLineSegs wireframe mat = do
    three <- jsg ("THREE" :: String)
    new (three ! ("LineSegments" :: String)) $ (wireframe, mat)

newBoxGeom :: Int -> Int -> Int -> JSM JSVal
newBoxGeom w h d = do
    three <- jsg ("THREE" :: String)
    new (three ! ("BoxGeometry" :: String)) $ (w, h, d)

newCam :: Int -> Float -> Float -> Int -> JSM JSVal
newCam fov aspect near far = do
    three <- jsg ("THREE" :: String)
    new (three ! ("PerspectiveCamera" :: String)) $ (fov, aspect, near, far)

newGroup :: JSM JSVal
newGroup = do
    three <- jsg ("THREE" :: String)
    new (three ! ("Group" :: String)) $ ()

newScene :: JSM JSVal
newScene = do
    three <- jsg ("THREE" :: String)
    new (three ! ("Scene" :: String)) $ ()

newSphereGeom :: Float -> JSM JSVal
newSphereGeom r = do
    three <- jsg ("THREE" :: String)
    new (three ! ("SphereGeometry" :: String)) $ ([r] :: [Float])

newWebglRenderer :: JSVal -> JSM JSVal
newWebglRenderer canv = do
    three <- jsg ("THREE" :: String)
    args <- obj
    setProp "canvas" canv args
    jsArgs <- toJSVal args
    new (three ! ("WebGLRenderer" :: String)) $ jsArgs

render :: JSVal -> JSVal -> JSVal -> JSM ()
render renderer scene cam = do
    void $ renderer # ("render" :: String) $ (scene, cam)

setName' :: JSVal -> String -> JSM ()
setName' obj' name = obj' <# ("name" :: String) $ name

setAxPos :: JSVal -> String -> Int -> JSM ()
setAxPos obj' ax pos = void $ obj' ! ("position" :: String) <# ax $ pos

setAxRot :: JSVal -> String -> Float -> JSM ()
setAxRot obj' ax rot = void $ obj' ! ("rotation" :: String) <# ax $ rot

setSize :: JSVal -> Float -> Float -> JSM ()
setSize renderer w h = void $ renderer # ("setSize" :: String) $ (w, h)
#endif
