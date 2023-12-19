module Driver.Driver.Www.Js.Ffi where


import GHCJS.Types


foreign import javascript "$1.add($2)"
    add :: JSVal -> JSVal -> IO ()

foreign import javascript "$1.children.length = 0"
    clearChildren :: JSVal -> IO ()

foreign import javascript "$1.getObjectByName($2)"
    getObjByName :: JSVal -> JSVal -> IO JSVal

foreign import javascript "new THREE.BoxGeometry($1, $2, $3)"
    newBoxGeom :: Int -> Int -> Int -> IO JSVal

foreign import javascript "new THREE.EdgesGeometry($1)"
    newEdgesGeom :: JSVal -> IO JSVal

foreign import javascript "new THREE.LineBasicMaterial({color: $1})"
    newLineBasMat :: JSVal -> IO JSVal

foreign import javascript "new THREE.LineSegments($1, $2)"
    newLineSegs :: JSVal -> JSVal -> IO JSVal

foreign import javascript "new THREE.SphereGeometry($1)"
    newSphereGeom :: Float -> IO JSVal

foreign import javascript "new THREE.PerspectiveCamera($1, $2, $3, $4)"
    newCam :: Int -> Float -> Float -> Int -> IO JSVal

foreign import javascript "new THREE.Group"
    newGroup :: IO JSVal

foreign import javascript "new THREE.Scene"
    newScene :: IO JSVal

foreign import javascript "new THREE.WebGLRenderer({canvas: $1})"
    newWebglRenderer :: JSVal -> IO JSVal

foreign import javascript "$1.render($2, $3)"
    render :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript "$1.name = $2"
    setName' :: JSVal -> JSString -> IO ()

foreign import javascript "$1.position[$2] = $3"
    setAxPos :: JSVal -> JSString -> Int -> IO ()

foreign import javascript "$1.rotation[$2] = $3"
    setAxRot :: JSVal -> JSString -> Float -> IO ()

foreign import javascript "$1.setSize($2, $3)"
    setSize :: JSVal -> Float -> Float -> IO ()
