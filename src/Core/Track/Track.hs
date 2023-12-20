{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}


module Core.Track.Track where


import Control.Lens
import Control.Monad.Free
import qualified Control.Monad.State as State
import Data.List
import Numeric.Natural
import System.Random

import Core.Configuration.Configuration
import Control.Monad.Reader
import Data.Monoid

import Core.Constraint


type PartLength = Natural

type ColumnIndex = Natural

type Track = Free Track' ()

type DifficultyLevel = Natural

type Difference = Int

type AmountDifference = Amount'

type Probability = Float

type Rise = Difference

type Run = PartLength

type PartBody = [[Cell]]

type Count = Natural

type RowIndex = Natural


newtype Boundaries = Boundaries {_un_ :: (ColumnIndex, ColumnIndex)}

newtype Range a = Range (a, a)

newtype Offset = Offset (ColumnIndex, ColumnIndex)


data Cell = Obstacle | TrailPart | Pass | Character | LivingEnemy | DeadEnemy
          deriving Eq

data GenerationState = GenerationState { _generator :: StdGen
                                       , _eitherSequences :: [Track]
                                       , _probabilities :: [Probability]
                                       , _cycle' :: Track
                                       , _repeatedSequences :: [(Count, Track)]
                                       , _markedSequence :: Maybe MarkedSequence
                                       , _sequenceEndsCount :: Count
                                       , _track :: State
                                       , _rowWithEnemies :: Bool
                                       }

data Track' next = Condition { _condition :: Condition
                             , _next :: next
                             }
                 | SequenceEnd { _next :: next
                               }
                 | Sequence { _sequence :: Sequence
                            , _next :: next
                            }
                 | Part { _part :: Part
                        , _next :: next
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

data Sequence = EitherSequenceWhere
              | InfiniteTailWhere
              | RepeatedSequenceWhere Count

data Part = StaticLengthFinitePart PartLength
          | MiddlePredefinedPart Cell PartBody
          | LeftPredefinedPart Cell PartBody
          | RightPredefinedPart Cell PartBody
          | DynamicLengthFinitePart (Range PartLength)

data MarkedSequence = EitherSequence | RepeatedSequence Count

data State = State { _difficulty :: Difficulty
                   , _rows :: [[Cell]]
                   , _name :: String
                   }


makeFieldsNoPrefix ''GenerationState
makeFieldsNoPrefix ''Boundaries
makeFieldsNoPrefix ''Difficulty
makeFieldsNoPrefix ''State


generateRow :: State.StateT GenerationState (Reader Configuration) ()
generateRow = do
    width <- fromIntegral <$> asks (^. options . trackWidth)
    startPartLength <- fromIntegral <$> asks (^. options . trackStartPartLength)
    trailPartColumnIndices <- selectNextTrailPartColumns
    newRow <- trail trailPartColumnIndices
           <$> (scatter Pass =<< lift generateObstacleRow)
    let middleCell = width `div` 2
        setCell cell index' = track . rows . _head . element index'' .= cell
          where
            index'' = fromIntegral index'
    forM_ trailPartColumnIndices $ setCell TrailPart
    enemiesCells <- use (track
                         . rows
                         . _head
                         . to (concat
                               . filter ((>= 3) . length)
                               . groupBy (\x y -> y == x + 1 || y == x + 2)
                               . findIndices (== TrailPart)
                              )
                        )
    parityEnemiesCells <- do
        rowWithEnemies' <- use rowWithEnemies
        rowWithEnemies %= not
        if rowWithEnemies'
        then do
            parityEnemiesCellsIs <- selectParities enemiesCells
            let parityEnemiesCells = enemiesCells
                                   ^.. traversed
                                   . ifiltered (\i _
                                                ->
                                                i `elem` parityEnemiesCellsIs
                                               )
            rowsCount <- use (track . rows . to length)
            return $ if rowsCount `elem` [startPartLength, startPartLength + 1]
                     then filter (`notElem` [ middleCell - 1
                                            , middleCell
                                            , middleCell + 1
                                            ]
                                 )
                                 parityEnemiesCells
                     else parityEnemiesCells
        else return []
    forM_ parityEnemiesCells $ setCell LivingEnemy
    track . rows %= (newRow :)

generateObstacleRow :: Reader Configuration [Cell]
generateObstacleRow = do
    width <- fromIntegral @Natural @Int <$> asks (^. options . trackWidth)
    return $ replicate width Obstacle

generateStartRow :: Reader Configuration [Cell]
generateStartRow = do
    width <- fromIntegral @Natural @Int <$> asks (^. options . trackWidth)
    let halfWidth = fromIntegral $ width `div` 2
    return $ replicate width Obstacle & element halfWidth .~ TrailPart

getShiftBoundaries :: ColumnIndex -> Reader Configuration Boundaries
getShiftBoundaries 0 = pure $ Boundaries (0, 1)
getShiftBoundaries position = do
    width <- asks (^. options . trackWidth)
    return $ if position == width - 1
             then Boundaries (position - 1, position)
             else Boundaries (position - 1, position + 1)

interpret :: Track -> StdGen -> State
interpret track' generator' = _track $ runReader initialise defaultConfiguration
  where
    initialise = do
         initialState <- initialiseState
         let initialGenerationState = initialiseGenerationState generator'
                                                                initialState
         State.execStateT (interpret' track') initialGenerationState

interpret' :: Track -> State.StateT GenerationState (Reader Configuration) ()
interpret' (Pure _) = pure ()
interpret' (Free (SequenceEnd track')) = do
    markedSequence' <- use markedSequence
    case markedSequence' of
        Just EitherSequence -> do
            eitherSequences' <- use eitherSequences
            eitherSequences .= []
            previousGenerator <- use generator
            probabilities' <- use probabilities
            probabilities .= []
            markedSequence .= Nothing
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
                                                     number <= to'
                                                     && number >= from'
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
        Just (RepeatedSequence _) -> do
            sequenceEndsCount %= (+ 1)
            sequenceEndsCount' <- fromIntegral @Natural @Int
                               <$> use sequenceEndsCount
            repeatedSequences' <- use repeatedSequences
            let repeatedSequencesCount = fromIntegral
                                       $ length repeatedSequences'
            when (sequenceEndsCount' == repeatedSequencesCount) $ do
                markedSequence .= Nothing
                repeatedSequences .= []
                sequenceEndsCount .= 0
                let repeatedSequence
                        = foldl (\sequence' (count', next')
                                 ->
                                 let count = fromIntegral count'
                                 in replicateM_ count $ next' *> sequence'
                                )
                                (Pure ())
                                repeatedSequences'
                interpret' repeatedSequence
        Nothing -> return ()
    interpret' track'
interpret' (Free (Sequence EitherSequenceWhere track')) = do
    eitherSequences %= (Pure () :)
    markedSequence .= Just EitherSequence
    interpret' track'
interpret' (Free (Condition (WithProbability probability') track')) = do
    probabilities %= (probability' :)
    interpret' track'
interpret' (Free (Sequence (RepeatedSequenceWhere count) track')) = do
    markedSequence .= Just (RepeatedSequence count)
    repeatedSequences %= ((count, Pure ()) :)
    interpret' track'
interpret' (Free track') = do
    markedSequence' <- use markedSequence
    case markedSequence' of
        Nothing -> do
            case track' of
                Part (StaticLengthFinitePart length') _ -> do
                    difficultyLevelSlope' <- use
                                           $ track . difficulty . levelSlope
                    case difficultyLevelSlope' of
                        GradualSlope rise run -> do
                            track . difficulty . levelSlope .= SteepSlope
                            let piecesCount = fromIntegral $ length' `div` run
                            interpret' . replicateM_ piecesCount $ do
                                withAlteredDifficultyLevel rise
                                staticLengthFinitePart run
                        SteepSlope
                            ->
                            replicateM_ (fromIntegral length') generateRow
                Condition (WithDifficultyLevel level') _ -> do
                    maximumDifficultyLevel <- asks (^. options . trackWidth)
                    track . difficulty . level
                          .= if level' <= maximumDifficultyLevel
                             then level'
                             else maximumDifficultyLevel
                Condition (WithAlteredDifficultyLevel difference) _ -> do
                    oldDifficultyLevel <- use $ track . difficulty . level
                    let newDifficultyLevel = fromIntegral oldDifficultyLevel
                                           + difference
                    maximumDifficultyLevel <- fromIntegral
                                           <$> asks (^. options . trackWidth)
                    interpret' $ do
                        withDifficultyLevel . fromIntegral
                                            $ if | newDifficultyLevel < 0 -> 0
                                                 | newDifficultyLevel
                                                   > maximumDifficultyLevel
                                                 -> maximumDifficultyLevel
                                                 | otherwise
                                                 -> newDifficultyLevel
                Condition (WithDifficultyLevelAmount amount') _ -> do
                    maximumDifficultyLevel <- asks (^. options . trackWidth)
                    interpret' . withDifficultyLevel
                               . round
                               $ fromIntegral maximumDifficultyLevel
                               * amount'
                Condition (WithAmountAlteredDifficultyLevel difference) _ -> do
                    maximumDifficultyLevel <- asks (^. options . trackWidth)
                    interpret' . withAlteredDifficultyLevel
                               . round
                               $ fromIntegral maximumDifficultyLevel
                               * if | abs difference < 0 -> 0
                                    | abs difference > 1 -> 1
                                    | otherwise -> difference
                Condition (WithGradualDifficultyLevelSlope rise run) _
                    ->
                    track . difficulty . levelSlope .= GradualSlope rise run
                Condition WithSteepDifficultyLevelSlope _
                    ->
                    track . difficulty . levelSlope .= SteepSlope
                Condition (WithGradualDifficultyLevelAmountRiseSlope rise run) _
                    -> do
                    maximumDifficultyLevel <- asks (^. options . trackWidth)
                    let rise' = round
                              $ fromIntegral maximumDifficultyLevel * rise
                    interpret' $ withGradualDifficultyLevelSlope rise' run
                Sequence InfiniteTailWhere cycle'' -> do
                    cycle' .= cycle''
                Part (MiddlePredefinedPart cell body) _ -> do
                    width <- fromIntegral <$> asks (^. options . trackWidth)
                    when (rectangular body && length (head body) <= width) $ do
                        let bodyOffset = fromIntegral
                                       $ width - length (head body)
                            leftBodyOffset = bodyOffset `div` 2
                            rightBodyOffset = bodyOffset - leftBodyOffset
                            offsettedBody = offsetBody cell
                                                       (Offset ( leftBodyOffset
                                                               , rightBodyOffset
                                                               )
                                                       )
                                                       body
                        track . rows %= (offsettedBody ++)
                Part (LeftPredefinedPart cell body) _ -> do
                    width <- fromIntegral <$> asks (^. options . trackWidth)
                    when (rectangular body && length (head body) <= width) $ do
                        let rightBodyOffset = fromIntegral
                                            $ width - length (head body)
                            rightOffsettedBody = offsetBody cell
                                                            (Offset ( 0
                                                                    , rightBodyOffset
                                                                    )
                                                            )
                                                            body
                        track . rows %= (rightOffsettedBody ++)
                Part (RightPredefinedPart cell body) _ -> do
                    width <- fromIntegral <$> asks (^. options . trackWidth)
                    when (rectangular body && length (head body) <= width) $ do
                        let leftBodyOffset = fromIntegral
                                           $ width - length (head body)
                            leftOffsettedBody = offsetBody cell
                                                           (Offset ( leftBodyOffset
                                                                   , 0
                                                                   )
                                                           )
                                                           body
                        track . rows %= (leftOffsettedBody ++)
                Part (DynamicLengthFinitePart (Range range)) _ -> do
                    previousGenerator <- use generator
                    let range' = range & each %~ fromIntegral @Natural @Int
                        (length', nextGenerator) = bimap (fromIntegral @Int
                                                                       @Natural                                                              
                                                         )
                                                         id
                                                         $ randomR range'
                                                                   previousGenerator
                    generator .= nextGenerator
                    interpret' $ staticLengthFinitePart length'
        Just EitherSequence -> do
            eitherSequences . _head %= (*> Free (Pure () <$ track'))
        Just (RepeatedSequence _) -> do
            lastIndex <- fromIntegral <$> use sequenceEndsCount
            repeatedSequences . ix lastIndex
                              . _2
                              %= (*> Free (Pure () <$ track'))

    case track' of
        Sequence InfiniteTailWhere _ -> return ()
        _ -> interpret' $ _next track'

selectNextTrailPartColumns :: State.StateT GenerationState
                                           (Reader Configuration)
                                           [ColumnIndex]
selectNextTrailPartColumns = do
    previouses <- ((fromIntegral <$>)
                   . findIndices (`elem` [TrailPart, Character])
                  )
               <$> use (track . rows . _head)
    is <- selectParities previouses
    let theForked = concatMap (\(i, previous)
                               ->
                               previous : (previous <$ guard (i `elem` is))
                              )
                              $ zip [0..length previouses - 1] previouses
    forM theForked $ \forked -> do
        shiftBoundaries <- lift
                        $ ((& each %~ fromIntegral @Natural @Int) . (^. un_))
                        <$> getShiftBoundaries forked
        previousGenerator' <- use generator
        let (next', nextGenerator'') = randomR shiftBoundaries
                                               previousGenerator'
        generator .= nextGenerator''
        return $ fromIntegral next'

staticLengthFinitePart :: PartLength -> Track
staticLengthFinitePart length'
    =
    Free (Part (StaticLengthFinitePart length') (Pure ()))

initialiseGenerationState :: StdGen -> State -> GenerationState
initialiseGenerationState generator' state
    =
    GenerationState generator' [] [] (Pure ()) [] Nothing 0 state True

generatePassPosition :: State.StateT GenerationState
                                     (Reader Configuration)
                                     ColumnIndex
generatePassPosition = do
    previousGenerator <- use generator
    width <- fromIntegral <$> asks (^. options . trackWidth)
    let firstColumn = 0 :: Int
        lastColumn = width - 1 :: Int
        (position, nextGenerator) = bimap (fromIntegral @Int @Natural)
                                          id
                                          $ randomR (firstColumn, lastColumn)
                                                    previousGenerator
    generator .= nextGenerator
    return position

scatter :: Cell
        -> [Cell]
        -> State.StateT GenerationState (Reader Configuration) [Cell]
scatter cell row = do
    difficultyLevel' <- use $ track . difficulty . level
    width <- asks (^. options . trackWidth)
    let passesCount = fromIntegral $ width - difficultyLevel'
    passPositions <- replicateM passesCount generatePassPosition
    return $ row & traversed
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

sequenceEnd :: Track
sequenceEnd = Free (SequenceEnd (Pure ()))

eitherSequenceWhere :: Track
eitherSequenceWhere = Free (Sequence EitherSequenceWhere (Pure ()))

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

infiniteTailWhere :: Track
infiniteTailWhere = Free (Sequence InfiniteTailWhere (Pure ()))

interpretFrom :: State -> Track -> StdGen -> State
interpretFrom state track' generator'
    =
    _track $ runReader initialise defaultConfiguration
  where
    initialise = State.execStateT (interpret' track') initialGenerationState'
    initialGenerationState' = initialiseGenerationState generator' state

middlePredefinedPart :: Cell -> PartBody -> Track
middlePredefinedPart cell body
    =
    Free ((Part (MiddlePredefinedPart cell body)) (Pure ()))

rectangular :: PartBody -> Bool
rectangular body = all ((== length (head body)) . length) body

leftPredefinedPart :: Cell -> PartBody -> Track
leftPredefinedPart cell body
    =
    Free ((Part (LeftPredefinedPart cell body)) (Pure ()))

offsetBody :: Cell -> Offset -> PartBody -> PartBody
offsetBody cell (Offset (left, right))
    =
    (^.. each . to ((++ replicate right'' cell) . (replicate left'' cell ++)))
  where
    left'' = fromIntegral left
    right'' = fromIntegral right

rightPredefinedPart :: Cell -> PartBody -> Track
rightPredefinedPart cell body
    =
    Free ((Part (RightPredefinedPart cell body)) (Pure ()))

repeatedSequenceWhere :: Count -> Track
repeatedSequenceWhere count
    =
    Free ((Sequence (RepeatedSequenceWhere count)) (Pure ()))

dynamicLengthFinitePart :: (PartLength, PartLength) -> Track
dynamicLengthFinitePart range
    =
    Free (Part (DynamicLengthFinitePart (Range range)) (Pure ()))

trail :: [ColumnIndex] -> [Cell] -> [Cell]
trail = flip
      $ foldr (\index' row
               ->
               let index'' = fromIntegral @Natural @Int index'
               in row & element index'' .~ TrailPart
              )

initialiseState :: Reader Configuration State
initialiseState = do
    startRow <- generateStartRow
    startPartLength' <- fromIntegral <$> asks (^. options . trackStartPartLength)
    let startPart = replicate startPartLength' startRow
    difficultyLevel' <- asks (^. options . trackDifficultyLevel)
    name' <- asks (^. preferences . trackName)
    return $ State (Difficulty difficultyLevel' SteepSlope) startPart name'

getCycle :: Track -> Track
getCycle track' = _cycle' $ runReader initialise defaultConfiguration
  where
    initialise = do
         initialState <- initialiseState
         let initialGenerationState = initialiseGenerationState generator'
                                                                initialState
         State.execStateT (interpret' track') initialGenerationState
    generator' = mkStdGen 0

selectParities :: [a]
               -> State.StateT GenerationState (Reader Configuration) [Int]
selectParities xs = do
    difficultyLevel' <- asks (^. options . trackDifficultyLevel)
    previousGenerator <- use generator
    width <- asks (^. options . trackWidth)
    let (currentCount, nextGenerator) = randomR ((0, maximumCount)
                                                 :: (Int, Int)
                                                )
                                                previousGenerator
        (parity, newGenerator') = randomR ((0, 1) :: (Int, Int)) nextGenerator
        maximumCount = fromIntegral $ width - difficultyLevel'
    generator .= newGenerator'
    ns <- replicateM currentCount $ do
        previousGenerator' <- use generator
        let (n, nextGenerator'') = randomR ((0, length xs `div` 2)
                                            :: (Int, Int)
                                           )
                                           previousGenerator'
        generator .= nextGenerator''
        return n
    let parityIs = map (subtract parity . (* 2)) ns
        parities = filter (`elem` parityIs) [0..length xs - 1]
    return parities
