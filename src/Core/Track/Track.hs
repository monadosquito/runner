{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}


module Core.Track.Track where


import qualified Lens.List.NonEmpty as List

import Control.Lens
import Control.Monad.Free
import Control.Monad.State
import Data.List
import Numeric.Natural
import System.Random
import qualified Data.List.NonEmpty as List
import qualified Data.Sequence as Sequence


type PartLength = Natural

type Position = Natural

type Track = Free Part ()

type Width = Natural


newtype Boundaries = Boundaries {_un_ :: (Position, Position)}


data Cell = Obstacle | TrailPart deriving Eq

data GenerationState = GenerationState { _cells :: List.NonEmpty [Cell]
                                       , _generator :: StdGen
                                       }

data Part next = Part PartLength next deriving Functor


makeFieldsNoPrefix ''GenerationState
makeFieldsNoPrefix ''Boundaries


generateLine :: State GenerationState ()
generateLine = do
    trailPartPosition <- selectNextTrailPartPosition
    case trailPartPosition of
        Just trailPartPosition' -> do
            cells . List._head
                  . element (fromIntegral trailPartPosition')
                  .= TrailPart
            let line = generateObstacleLine
                     & element (fromIntegral trailPartPosition')
                     .~ TrailPart
            cells %= (line List.<|)
        Nothing -> return ()

generateObstacleLine :: [Cell]
generateObstacleLine = replicate (fromIntegral width) Obstacle

generateStartLine :: [Cell]
generateStartLine = replicate (fromIntegral width) Obstacle
                  & element (fromIntegral $ width `div` 2)
                  .~ TrailPart

getShiftBoundaries :: Position -> Boundaries
getShiftBoundaries 0 = Boundaries (0, 1)
getShiftBoundaries position
    | position == width - 1 = Boundaries (position - 1, position)
    | otherwise = Boundaries (position - 1, position + 1)

interpret :: StdGen -> Track -> List.NonEmpty [Cell]
interpret generator' track
    =
    _cells $ execState (interpret' track) initialGenerationState
  where
    initialGenerationState
        =
        GenerationState (pure generateStartLine) generator'

interpret' :: Track -> State GenerationState ()
interpret' (Pure _) = pure ()
interpret' (Free (Part length' track))
    =
    Sequence.replicateA (fromIntegral length') generateLine *> interpret' track

selectNextTrailPartPosition :: State GenerationState (Maybe Position)
selectNextTrailPartPosition = do
    previous <- ((fromIntegral <$>) . findIndex (== TrailPart))
             <$> use (cells . List._head)
    case previous of
        Just previous' -> do
            previousGenerator <- use generator
            let shiftBoundaries = getShiftBoundaries previous'
                                ^. un_
                                & each
                                %~ fromIntegral @Natural @Int
                (next', nextGenerator) = randomR shiftBoundaries
                                                 previousGenerator
            generator .= nextGenerator
            return $ Just (fromIntegral next')
        Nothing -> return Nothing

part :: PartLength -> Track
part length' = Free (Part length' (Pure ()))

width :: Width
width = 5
