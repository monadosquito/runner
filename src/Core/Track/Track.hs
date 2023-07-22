{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}


module Core.Track.Track where


import qualified Lens.List.NonEmpty as List

import Control.Lens
import Control.Monad.Free
import Control.Monad.State
import Data.List
import Numeric.Natural
import System.Random
import qualified Data.List.NonEmpty as List

import Core.Configuration.Configuration
import Control.Monad.Reader
import Data.Monoid

import Core.Constraint


type PartLength = Natural

type Position = Natural

type Track = Free Track' ()

type DifficultyLevel = Natural

type Difference = Int

type AmountDifference = Amount'

type Probability = Float

type Rise = Difference

type Run = PartLength


newtype Boundaries = Boundaries {_un_ :: (Position, Position)}

newtype Range = Range (Probability, Probability)


data Cell = Obstacle | TrailPart | Pass deriving Eq

data GenerationState = GenerationState { _cells :: List.NonEmpty [Cell]
                                       , _generator :: StdGen
                                       , _difficulty :: Difficulty
                                       , _eitherSequences :: [Track]
                                       , _probabilities :: [Probability]
                                       }

data Track' next = Part { _partLength :: PartLength
                        , _next :: next
                        }
                 | Condition { _condition :: Condition
                             , _next :: next
                             }
                 | EitherSequenceEnd { _next :: next
                                     }
                 | EitherSequenceWhere { _next :: next
                                       }
                 deriving Functor

data Condition = WithDifficultyLevel DifficultyLevel
               | WithAlteredDifficultyLevel Difference
               | WithDifficultyLevelAmount Amount
               | WithAmountAlteredDifficultyLevel Amount'
               | WithProbability Probability
               | WithGradualDifficultyLevelSlope Rise Run
               | WithSteepDifficultyLevelSlope
               | WithGradualDifficultyLevelAmountRiseSlope AmountRise Run

data Difficulty = Difficulty { _level :: DifficultyLevel
                             , _levelSlope :: Slope
                             }

data Slope = GradualSlope Rise Run | SteepSlope


makeFieldsNoPrefix ''GenerationState
makeFieldsNoPrefix ''Boundaries
makeFieldsNoPrefix ''Difficulty


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
interpret' (Free (EitherSequenceEnd track)) = do
    eitherSequences' <- use eitherSequences
    eitherSequences .= []
    previousGenerator <- use generator
    probabilities' <- use probabilities
    probabilities .= []
    if length eitherSequences' == length probabilities'
       && foldMap Sum probabilities' == 1
    then do
        let (number, nextGenerator) = randomR ((0, 1) :: (Float, Float))
                                              previousGenerator
            probabilityRanges = snd
                              $ mapAccumL (\from' to'
                                           ->
                                           ( from' + to'
                                           , Range (from', from' + to')
                                           )
                                          )
                                          0
                                          probabilities'
            eitherSequenceIndex = findIndex (\(Range (from', to'))
                                             ->
                                             number <= to' && number >= from'
                                            )
                                            probabilityRanges
        generator .= nextGenerator
        case eitherSequenceIndex of
            Just eitherSequenceIndex'
                ->
                interpret' $ eitherSequences' !! eitherSequenceIndex'
            Nothing -> return ()
    else do
        let (eitherSequenceIndex, nextGenerator) = randomR ( 0
                                                           , length eitherSequences'
                                                             - 1
                                                           )
                                                           previousGenerator
        generator .= nextGenerator
        interpret' $ eitherSequences' !! eitherSequenceIndex
    interpret' track
interpret' (Free (EitherSequenceWhere track)) = do
    eitherSequences %= (Pure () :)
    interpret' track
interpret' (Free (Condition (WithProbability probability') track)) = do
    probabilities %= (probability' :)
    interpret' track
interpret' (Free track) = do
    eitherSequences' <- use eitherSequences
    if null eitherSequences'
    then do
        case track of
            Part length' _ -> do
                difficultyLevelSlope' <- use $ difficulty . levelSlope
                case difficultyLevelSlope' of
                    GradualSlope rise run -> do
                        width <- asks _trackWidth
                        if length' `div` run * fromIntegral rise <= width
                        then do
                            difficulty . levelSlope .= SteepSlope
                            let piecesCount = fromIntegral $ length' `div` run
                            interpret' . replicateM_ piecesCount $ do
                                withAlteredDifficultyLevel rise
                                part run
                            difficulty . levelSlope .= difficultyLevelSlope'
                        else do
                            difficulty . levelSlope .= SteepSlope
                            interpret' $ part length'
                    SteepSlope
                        ->
                        replicateM_ (fromIntegral length') generateLine
            Condition (WithDifficultyLevel level') _ -> do
                maximumDifficultyLevel <- asks _trackWidth
                difficulty . level .= if level' <= maximumDifficultyLevel
                                      then level'
                                      else maximumDifficultyLevel
            Condition (WithAlteredDifficultyLevel difference) _ -> do
                oldDifficultyLevel <- use $ difficulty . level
                let newDifficultyLevel = fromIntegral oldDifficultyLevel
                                       + difference
                maximumDifficultyLevel <- fromIntegral <$> asks _trackWidth
                interpret' $ do
                    withDifficultyLevel . fromIntegral
                                        $ if | newDifficultyLevel < 0 -> 0
                                             | newDifficultyLevel
                                               > maximumDifficultyLevel
                                             -> maximumDifficultyLevel
                                             | otherwise -> newDifficultyLevel
            Condition (WithDifficultyLevelAmount amount') _ -> do
                maximumDifficultyLevel <- asks _trackWidth
                interpret' . withDifficultyLevel
                           . round
                           $ fromIntegral maximumDifficultyLevel
                           * amount'
            Condition (WithAmountAlteredDifficultyLevel difference) _ -> do
                maximumDifficultyLevel <- asks _trackWidth
                interpret' . withAlteredDifficultyLevel
                           . round
                           $ fromIntegral maximumDifficultyLevel
                           * if | abs difference < 0 -> 0
                                | abs difference > 1 -> 1
                                | otherwise -> difference
            Condition (WithGradualDifficultyLevelSlope rise run) _
                ->
                difficulty . levelSlope .= GradualSlope rise run
            Condition WithSteepDifficultyLevelSlope _
                ->
                difficulty . levelSlope .= SteepSlope
            Condition (WithGradualDifficultyLevelAmountRiseSlope rise run) _
                -> do
                maximumDifficultyLevel <- asks _trackWidth
                let rise' = round $ fromIntegral maximumDifficultyLevel * rise
                interpret' $ withGradualDifficultyLevelSlope rise' run
    else eitherSequences . _head %= (*> Free (Pure () <$ track))
    interpret' $ _next track

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
                             (Difficulty difficultyLevel' SteepSlope)
                             []
                             []

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
    difficultyLevel' <- use $ difficulty . level
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
withDifficultyLevel level'
    =
    Free (Condition (WithDifficultyLevel level') (Pure ()))

withAlteredDifficultyLevel :: Difference -> Track
withAlteredDifficultyLevel difference
    =
    Free (Condition (WithAlteredDifficultyLevel difference) (Pure ()))

withDifficultyLevelAmount :: Amount' -> Track
withDifficultyLevelAmount amount'
    =
    Free (Condition (WithDifficultyLevelAmount (amount amount')) (Pure ()))

withAmountAlteredDifficultyLevel :: AmountDifference -> Track
withAmountAlteredDifficultyLevel difference
    =
    Free (Condition (WithAmountAlteredDifficultyLevel difference) (Pure ()))

eitherSequenceEnd :: Track
eitherSequenceEnd = Free (EitherSequenceEnd (Pure ()))

eitherSequenceWhere :: Track
eitherSequenceWhere = Free (EitherSequenceWhere (Pure ()))

withProbability :: Probability -> Track
withProbability probability
    =
    Free (Condition (WithProbability probability) (Pure ()))

withGradualDifficultyLevelSlope :: Rise -> Run -> Track
withGradualDifficultyLevelSlope rise run
    =
    Free (Condition (WithGradualDifficultyLevelSlope rise run) (Pure ()))

withSteepDifficultyLevelSlope :: Track
withSteepDifficultyLevelSlope
    =
    Free (Condition (WithSteepDifficultyLevelSlope) (Pure ()))

withGradualDifficultyLevelAmountRiseSlope :: AmountRise' -> Run -> Track
withGradualDifficultyLevelAmountRiseSlope rise run
    =
    Free (Condition (WithGradualDifficultyLevelAmountRiseSlope (amountRise rise)
                                                               run
                    )
                    (Pure ())
         )
