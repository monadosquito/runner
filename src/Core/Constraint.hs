{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Core.Constraint ( Amount
                       , Amount'
                       , amount
                       ) where



type Amount' = Float


newtype Amount = Amount Amount'
               deriving newtype (Eq, Fractional, Num, Ord, Real, RealFrac)


amount :: Amount' -> Amount
amount amount' | amount' < 0 = Amount 0
               | amount' > 1 = Amount 1
               | otherwise = Amount amount'
