-- |
-- Module      :  Christopher.Simulation
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines what a financial planning simulation is and how to run it

module Christopher.Simulation
(
  Simulation,
  SimulationInfo(..),
  SimulationResults,
  Assets,
  YearlyData(..),
  SState,
  Strategy(..),
  runSimulation
)
where

import Data.Decimal
import Data.Functor.Foldable
import Control.Monad.State
import Christopher.Taxes
import Christopher.Markets

data SimulationInfo = SimulationInfo {
  infoAssets :: Assets,
  infoYearlyData :: [YearlyData]
} deriving (Show, Eq)

type SimulationResults = Assets

type Assets = Asset

data YearlyData = YearlyData {
  ydIncome :: Income, -- Before income tax
  ydNeedForSpending :: Decimal, -- Before income tax. As salary.
  ydIncomeTaxInfo :: IncomeTaxInfo,
  ydYield :: Rate
} deriving (Show, Eq)

type SState = State Assets

data Strategy = Strategy {
  sName :: String,
  sPayTaxesAndInvest :: (Income, Decimal) -> IncomeTaxInfo -> SState ()
} 

instance Show Strategy where
  show s = show (sName s)

type Simulation = (Strategy, SimulationInfo)

runSimulation :: Simulation -> SimulationResults
runSimulation (strat, info) = 
  let simul = cata alg (infoYearlyData info)
  in evalState simul (infoAssets info)

  where
    alg :: ListF YearlyData (SState SimulationResults) -> (SState SimulationResults)
    alg Nil = get >>= pure
    alg (Cons yearlyData next) = do
      sPayTaxesAndInvest strat (ydIncome yearlyData, ydNeedForSpending yearlyData) (ydIncomeTaxInfo yearlyData)
      simulateMarket (ydYield yearlyData)
      next

simulateMarket :: Rate -> SState ()
simulateMarket r = do
  a <- get
  let newBalance = roundTo 2 (aBalance a *. (1 + r))
  put a{aBalance = newBalance}