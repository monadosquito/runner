module Lens.List.NonEmpty where


import Control.Lens
import qualified Data.List.NonEmpty as NonEmpty


_head :: Lens' (NonEmpty.NonEmpty a) a
_head = lens NonEmpty.head $ \(_ NonEmpty.:| xs) x -> x NonEmpty.:| xs
