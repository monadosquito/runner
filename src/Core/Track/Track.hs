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

import Core.Configuration.Configuration
import Control.Monad.Reader


type PartLength = Natural

type Position = Natural

type Track = Free Track' ()

type DifficultyLevel = Natural


newtype Boundaries = Boundaries {_un_ :: (Position, Position)}


data Cell = Obstacle | TrailPart | Pass deriving Eq

data GenerationState = GenerationState { _cells :: List.NonEmpty [Cell]
                                       , _generator :: StdGen
                                       , _difficultyLevel :: DifficultyLevel
                                       }

data Track' next = Part PartLength next
                 | WithDifficultyLevel DifficultyLevel next
                 deriving Functor


makeFieldsNoPrefix ''GenerationState
makeFieldsNoPrefix ''Boundaries


generateLine :: StateT GenerationState (Reader Options) ()
generateLine = do
    trailPartPosition <- selectNextTrailPartPosition
    case trailPartPosition of
        Just trailPartPosition' -> do
            cells . List._head
                  . element (fromIntegral trailPartPosition')
                  .= TrailPart
            line <- (& element (fromIntegral trailPartPosition') .~ TrailPart)
                 <$> (lift generateObstacleLine >>= scatter Pass)
            cells %= (line List.<|)
        Nothing -> return ()

generateObstacleLine :: Reader Options [Cell]
generateObstacleLine = do
    width <- asks _trackWidth
    return $ replicate (fromIntegral width) Obstacle

generateStartLine :: Reader Options [Cell]
generateStartLine = do
    width <- asks _trackWidth
    return $ replicate (fromIntegral width) Obstacle
           & element (fromIntegral $ width `div` 2)
           .~ TrailPart

getShiftBoundaries :: Position -> Reader Options Boundaries
getShiftBoundaries 0 = pure $ Boundaries (0, 1)
getShiftBoundaries position = do
    width <- asks _trackWidth
    return $ if position == width - 1
             then Boundaries (position - 1, position)
             else Boundaries (position - 1, position + 1)

interpret :: StdGen -> Track -> List.NonEmpty [Cell]
interpret generator' track = _cells $ runReader initialise defaultOptions
  where
    initialise = do
         generationState <- initialGenerationState generator'
         execStateT (interpret' track) generationState

interpret' :: Track -> StateT GenerationState (Reader Options) ()
interpret' (Pure _) = pure ()
interpret' (Free (Part length' track))
    =
    Sequence.replicateA (fromIntegral length') generateLine *> interpret' track
interpret' (Free (WithDifficultyLevel level track)) = do
    maximumDifficultyLevel <- asks _trackWidth
    difficultyLevel .= if level <= maximumDifficultyLevel
                       then level
                       else maximumDifficultyLevel
    interpret' track

selectNextTrailPartPosition :: StateT GenerationState (Reader Options)
                                                      (Maybe Position)
selectNextTrailPartPosition = do
    previous <- ((fromIntegral <$>) . findIndex (== TrailPart))
             <$> use (cells . List._head)
    case previous of
        Just previous' -> do
            shiftBoundaries <- lift
                            $ ((& each %~ fromIntegral @Natural @Int)
                               . (^. un_)
                              )
                            <$> getShiftBoundaries previous'
            previousGenerator <- use generator
            let (next', nextGenerator) = randomR shiftBoundaries
                                                 previousGenerator
            generator .= nextGenerator
            return $ Just (fromIntegral next')
        Nothing -> return Nothing

part :: PartLength -> Track
part length' = Free (Part length' (Pure ()))

initialGenerationState :: StdGen -> Reader Options GenerationState
initialGenerationState generator' = do
    startLine <- generateStartLine
    difficultyLevel' <- asks _trackDifficultyLevel
    return $ GenerationState (pure startLine)
                             generator'
                             (fromIntegral difficultyLevel')

generatePassPosition :: StateT GenerationState (Reader Options) Position
generatePassPosition = do
    previousGenerator <- use generator
    width <- asks _trackWidth
    let (position, nextGenerator) = randomR (0 :: Int, fromIntegral $ width - 1)
                                            previousGenerator
    generator .= nextGenerator
    return $ fromIntegral position

scatter :: Cell -> [Cell] -> StateT GenerationState (Reader Options) [Cell]
scatter cell line = do
    difficultyLevel' <- use difficultyLevel
    width <- asks _trackWidth
    passPositions <- replicateM (fromIntegral $ width - difficultyLevel')
                                generatePassPosition
    return $ line & traversed
                  . withIndex
                  . filteredBy (_1
                                . to ((`find` passPositions)
                                      . (==)
                                      . fromIntegral
                                     )
                                . _Just
                               )
                  <. _2
                  .~ cell

withDifficultyLevel :: DifficultyLevel -> Track
withDifficultyLevel level = Free (WithDifficultyLevel level (Pure ()))
