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
  Assets(..),
  MarketInput,
  Rate,
  simulateMarket,
  marketPnl
)
where

import Data.Decimal

-- For the purposes of financial planning, three types of financial account
-- exists. We can invest in each of them.
data Assets = Assets {
  aRrsp :: Decimal,
  aTfsa :: Decimal,
  aNonRegistered :: Decimal
} deriving (Show, Eq)

type Rate = Rational
type MarketInput = Rate

simulateMarket :: MarketInput -> Assets -> Assets
simulateMarket r a = 
  let newRrsp = roundTo 2 (aRrsp a *. (1 + r))
      newTfsa = roundTo 2 (aTfsa a *. (1 + r))
      newNonRegistered = roundTo 2 (aNonRegistered a *. (1 + r))
  in Assets newRrsp newTfsa newNonRegistered

marketPnl :: Assets -> Assets -> Assets
marketPnl a b = Assets (aRrsp b - aRrsp a)
                       (aTfsa b - aTfsa a)
                       (aNonRegistered b - aNonRegistered a)