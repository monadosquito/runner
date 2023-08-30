module Core.Port.Renderer where


import Core.Track.Track

import Data.Proxy


class Renderer r where
    render :: Proxy r -> [[Cell]] -> IO ()
