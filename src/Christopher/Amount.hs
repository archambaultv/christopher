{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  Christopher.Amount
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines what an amount is.

module Christopher.Amount
(
  Rate,
  showRate,
  Amount(..),
  RAmount,
  unitToAmount,
  amountToRational,
  showAmount,
  fromScientific,
  toScientific,
  roundAwayFromZero,
  truncateAmount,
  (-.),
  (+.),
  (*.),
  (.*),
  (%.)
)
where

import Data.Ratio ((%))
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Csv (ToField(..), FromField(..))

type Rate = Rational
type RAmount = Rational
-- Multiplication of money by money does not make sense, so we do not defined num class
-- Money is a monoid
newtype Amount = Amount Integer
  deriving (Eq, Ord, Enum, Show, Read)

instance Semigroup Amount where
  (Amount a1) <> (Amount a2) = (Amount $ a1 + a2)

instance Monoid Amount where
  mempty = Amount 0

unitToAmount :: Integer -> Amount
unitToAmount x = Amount (x * 100)

amountToInteger :: Amount -> Integer
amountToInteger (Amount x) = x

amountToRational :: Amount -> Rational
amountToRational (Amount x) = toRational x

fromScientific :: Scientific -> Maybe Amount
fromScientific x = 
  case floatingOrInteger (x * 100) :: Either Double Integer of
    Right y -> Just (Amount y)
    _ -> Nothing

toScientific :: Amount -> Scientific
toScientific = fromRational .  (/ 100) . toRational . amountToInteger

instance ToJSON Amount where
  toJSON = toJSON . toScientific
  toEncoding = toEncoding . toScientific

instance FromJSON Amount where
    parseJSON a = do
      x <- parseJSON a
      case fromScientific x of
        Just y -> return y
        Nothing -> fail "Number is not a valid currency (max 2 decimal places)"

instance ToField Amount where
  toField = toField . toScientific

instance FromField Amount where
  parseField a = do
      x <- parseField a
      case fromScientific x of
        Just y -> return y
        Nothing -> fail "Number is not a valid currency (max 2 decimal places)"

roundAwayFromZero :: Rational -> Amount
roundAwayFromZero x =
  let sign :: Integer
      sign = truncate (signum x)
      ax :: Rational
      ax = abs x
      intPart :: Integer
      intPart = truncate ax
      fracPart :: Rational
      fracPart = ax - toRational intPart
  in if fracPart < 1 % 2
     then Amount $ sign * intPart
     else Amount $ sign * (intPart + 1)

truncateAmount :: Rational -> Amount
truncateAmount x = Amount (truncate x)

infixl 6 +.
(+.) :: Amount -> Amount -> Amount
(+.) = (<>)

infixl 6 -.
(-.) :: Amount -> Amount -> Amount
(-.) (Amount a1) (Amount a2) = (Amount $ a1 - a2)

(%.) :: Amount -> Amount -> Rational
(%.) (Amount a1) (Amount a2) = a1 % a2

(*.) :: Amount -> Rational -> Rational
(*.) (Amount x) r = toRational x * r

(.*) :: Integer -> Amount -> Amount
(.*) i (Amount x) = Amount (i * x)

-- | Formats a number for reporting
showAmount :: Char -> Amount -> String
showAmount decimalSeparator = 
  map (\x -> if x == '.' then decimalSeparator else x) . show . toScientific

-- | Formats a rate for reporting
showRate :: Char -> Rate -> String
showRate decimalSeparator = 
  map (\x -> if x == '.' then decimalSeparator else x) . (show :: Double -> String) . fromRational