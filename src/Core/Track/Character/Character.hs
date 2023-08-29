module Core.Track.Character.Character where


import Core.Character.Character
import Core.Track.Track

import Control.Lens


reflect :: Position -> [[Cell]] -> [[Cell]]
reflect (Position position) rows'
    =
    rows' & element rowIndex . element columnIndex .~ Character
  where
    rowIndex = fromIntegral $ fst position
    columnIndex = fromIntegral $ snd position
