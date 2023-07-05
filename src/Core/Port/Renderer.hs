module Core.Port.Renderer where


import Core.Track.Track

import Data.List.NonEmpty
import Data.Proxy


class Renderer r where
    render :: Proxy r -> NonEmpty [Cell] -> IO ()
