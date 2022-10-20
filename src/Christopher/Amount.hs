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
  Amount,
  Rate,
  roundTo,
  (*.),
  fromScientific,
  toScientific,
  toRate,
  fromScientificR,
  toScientificR,
  showAmount,
  showRate
)
where

import Data.Word
import Data.Scientific (Scientific)
import Data.Decimal (Decimal)
import qualified Data.Decimal as D
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Csv (ToField(..), FromField(..))

newtype Amount = Amount Decimal
  deriving (Eq, Ord, Enum, Show, Read, Num, Fractional, Real, RealFrac)

instance ToJSON Amount where
  toJSON = toJSON . toScientific
  toEncoding = toEncoding . toScientific

instance FromJSON Amount where
    parseJSON = fmap fromScientific . parseJSON

instance ToField Amount where
  toField = toField . toScientific

instance FromField Amount where
  parseField = fmap fromScientific . parseField

newtype Rate = Rate Rational
  deriving (Eq, Ord, Enum, Show, Read, Num, Fractional, ToJSON, FromJSON)

instance ToField Rate where
  toField = toField . toScientificR

instance FromField Rate where
  parseField = fmap fromScientificR . parseField

roundTo :: Word8 -> Amount -> Amount
roundTo i (Amount x) = Amount (D.roundTo i x)

(*.) :: Amount -> Rate -> Amount
(*.) (Amount x) (Rate r) = Amount $ x D.*. r

fromScientific :: Scientific -> Amount
fromScientific = fromRational . toRational

toScientific :: Amount -> Scientific
toScientific = fromRational . toRational

toRate :: Amount -> Rate
toRate = Rate . toRational

fromScientificR :: Scientific -> Rate
fromScientificR = Rate . toRational

toScientificR :: Rate -> Scientific
toScientificR (Rate x)= fromRational x

-- | Formats a number for reporting
showAmount :: Char -> Amount -> String
showAmount decimalSeparator = 
  map (\x -> if x == '.' then decimalSeparator else x) . show . toScientific

-- | Formats a rate for reporting
showRate :: Char -> Rate -> String
showRate decimalSeparator = 
  map (\x -> if x == '.' then decimalSeparator else x) . show . toScientificR