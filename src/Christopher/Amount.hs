{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  Christopher.Amount
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines what an amount is. All computatations are always rounded to
-- the fixed number of decimal place

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

newtype Amount = Amount Decimal
  deriving (Eq, Ord, Enum, Show, Read, Num, Fractional, Real, RealFrac)

instance ToJSON Amount where
  toJSON x = toJSON (toScientific x)
  toEncoding x = toEncoding (toScientific x)

instance FromJSON Amount where
    parseJSON = \v -> fmap fromScientific (parseJSON v)

type Rate = Rational

roundTo :: Word8 -> Amount -> Amount
roundTo i (Amount x) = Amount (D.roundTo i x)

(*.) :: Amount -> Rate -> Amount
(*.) (Amount x) r = Amount $ x D.*. r

fromScientific :: Scientific -> Amount
fromScientific = fromRational . toRational

toScientific :: Amount -> Scientific
toScientific = fromRational . toRational

toRate :: Amount -> Rate
toRate = toRational

fromScientificR :: Scientific -> Rate
fromScientificR = toRational

toScientificR :: Rate -> Scientific
toScientificR = fromRational 

-- | Formats a number for reporting
showAmount :: Char -> Amount -> String
showAmount decimalSeparator = 
  map (\x -> if x == '.' then decimalSeparator else x) . show

-- | Formats a rate for reporting
showRate :: Char -> Rate -> String
showRate decimalSeparator = 
  map (\x -> if x == '.' then decimalSeparator else x) . show . toScientificR