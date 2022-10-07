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
  Asset(..),
  AccountType(..),
  Rate,
  YearlyDatum(..),
  FiscalDatum(..),
  TaxBrackets(..),
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
  infoYearlyData :: [YearlyDatum]
} deriving (Show, Eq)

type SimulationResults = Assets

type Assets = Asset

data YearlyDatum = YearlyDatum {
  fiscalDatum :: FiscalDatum,
  yield :: Rate
} deriving (Show, Eq)

data FiscalDatum = FiscalDatum {
  income :: Decimal, -- Before income tax amount
  needForSpending :: Decimal, -- Before income tax amount
  taxBrackets :: TaxBrackets
} deriving (Show, Eq)

type SState = State Assets

data Strategy = Strategy {
  sName :: String,
  sPayTaxesAndInvest :: FiscalDatum -> SState ()
} 

instance Show Strategy where
  show s = show (sName s)

type Simulation = (Strategy, SimulationInfo)

runSimulation :: Simulation -> SimulationResults
runSimulation (strat, info) = 
  let simul = cata alg (infoYearlyData info)
  in evalState simul (infoAssets info)

  where
    alg :: ListF YearlyDatum (SState SimulationResults) -> (SState SimulationResults)
    alg Nil = get >>= pure
    alg (Cons datum next) = do
      sPayTaxesAndInvest strat (fiscalDatum datum)
      simulateMarket (yield datum)
      next

simulateMarket :: Rate -> SState ()
simulateMarket r = do
  a <- get
  let newBalance = roundTo 2 (aBalance a *. (1 + r))
  put a{aBalance = newBalance}