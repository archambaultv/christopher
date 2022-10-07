-- |
-- Module      :  Christopher.Markets
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines the financial instruments and how to simulate the financial markets

module Christopher.Markets
(
  AccountType(..),
  Asset(..),
  Rate
)
where

import Data.Decimal

data AccountType 
  = RRSP -- REER
  | TFSA -- CELI
  deriving (Show, Eq)

data Asset = Asset {
  aType :: AccountType,
  aBalance :: Decimal
} deriving (Show, Eq)

type Rate = Rational