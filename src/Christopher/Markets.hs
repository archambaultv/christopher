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
  addToRrsp,
  addToTfsa,
  addToBank,
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
  aBank :: Decimal -- Money sitting there, doing nothing
} deriving (Show, Eq)

type Rate = Rational
type MarketInput = Rate

addToRrsp :: Assets -> Decimal -> Assets
addToRrsp (Assets rrsp a b) x = Assets (rrsp + x) a b

addToTfsa :: Assets -> Decimal -> Assets
addToTfsa(Assets a tfsa b) x = Assets a (tfsa + x) b

addToBank :: Assets -> Decimal -> Assets
addToBank(Assets a b bank) x = Assets a b (bank + x)

simulateMarket :: MarketInput -> Assets -> Assets
simulateMarket r a = 
  let newRrsp = roundTo 2 (aRrsp a *. (1 + r))
      newTfsa = roundTo 2 (aTfsa a *. (1 + r))
  in Assets newRrsp newTfsa (aBank a)

marketPnl :: Assets -> Assets -> Assets
marketPnl a b = Assets (aRrsp b - aRrsp a)
                       (aTfsa b - aTfsa a)
                       (aBank b - aBank a)