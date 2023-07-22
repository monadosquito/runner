{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Core.Constraint ( Amount
                       , Amount'
                       , amount
                       , AmountRise
                       , AmountRise'
                       , amountRise
                       ) where



type Amount' = Float

type AmountRise' = Amount'


newtype Amount = Amount Amount'
               deriving newtype (Eq, Fractional, Num, Ord, Real, RealFrac)

newtype AmountRise = AmountRise AmountRise'
               deriving newtype (Eq, Fractional, Num, Ord, Real, RealFrac)


amount :: Amount' -> Amount
amount amount' | amount' < 0 = Amount 0
               | amount' > 1 = Amount 1
               | otherwise = Amount amount'

amountRise :: AmountRise' -> AmountRise
amountRise rise | rise < -1 = AmountRise (-1)
                | rise > 1 = AmountRise 1
                | otherwise = AmountRise rise
