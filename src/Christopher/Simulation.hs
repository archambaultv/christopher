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
  YearlyInput(..),
  SState,
  Strategy(..),
  runSimulation,
  resultsToReport
)
where

import Data.Decimal
import Data.List (foldl')
import Data.Functor.Foldable
import Control.Monad.State
import Christopher.Taxes
import Christopher.Markets

data SimulationInfo = SimulationInfo {
  infoStartingAssets :: Assets, -- Assets a the first yearly input
  infoStartingYear :: Int, -- Year of the first yearly input
  infoStartingAge :: Int, -- Age at the first yearly input
  infoYearlyData :: [YearlyInput]
} deriving (Show, Eq)

data YearlyInput = YearlyInput {
  yiFinancialInput :: FinancialInput, -- Things known to the investors
  yiMarketInput :: MarketInput -- Things unknown to the investors
} deriving (Show, Eq)

data FinancialInput = FinancialInput {
  fiIncome :: Income, -- Before income tax
  fiNeedForSpending :: Decimal, -- Before income tax as a salary (no dividend)
  fiIncomeTaxInfo :: IncomeTaxInfo
} deriving (Show, Eq)

data SimulationState = SimulationState {
  ssYear :: Int,
  ssAge :: Int,
  ssCurrentYear :: AssetsState,
  ssPreviousYear :: [(YearlyInput, AssetsState)]
} deriving (Show, Eq)

-- Everything that a strategy can have acces to and possibly modify
data AssetsState = AssetsState {
  asAssets :: Assets,
  asRRSPContrib :: Decimal,
  asRRSPMarketPnl :: Decimal,
  asTFSAContrib :: Decimal,
  asTFSAMarketPnl :: Decimal
} deriving (Show, Eq)

initialSimulationState :: Int -> Int -> Assets -> SimulationState
initialSimulationState year age assets = 
  SimulationState year age (AssetsState assets 0 0 0 0) []

type SState = State SimulationState

data Strategy = Strategy {
  sName :: String,
  -- Use the data available in the state to make sure we have enough for
  -- needForSpending, pay the taxes and then invest the surplus.
  sPayTaxesAndInvest :: FinancialInput -> SState ()
} 

instance Show Strategy where
  show s = show (sName s)

type Simulation = (Strategy, SimulationInfo)
data SimulationResults = SimulationResults {
  srYear :: Int,
  srAge :: Int,
  srData :: [(YearlyInput, AssetsState)]
}

runSimulation :: Simulation -> SimulationResults
runSimulation (strat, info) = 
  let s0 = initialSimulationState (infoStartingYear info)
                                  (infoStartingAge info)
                                  (infoStartingAssets info)
      simul :: SState SimulationResults
      simul = cata alg (infoYearlyData info)
  in evalState simul s0

  where
    alg :: ListF YearlyInput (SState SimulationResults) -> (SState SimulationResults)
    -- At the end, we return the report Stata
    alg Nil = do
      s <- get
      let age = ssAge s - 1
      let year = ssYear s - 1
      let ds = ssPreviousYear s
      pure $ SimulationResults year age ds
    alg (Cons yearlyInput next) = do
      sPayTaxesAndInvest strat (yiFinancialInput yearlyInput) 
      simulateMarket' (yiMarketInput yearlyInput)
      endOfYear yearlyInput
      next

simulateMarket' :: MarketInput -> SState ()
simulateMarket' mi = do
  s <- get
  let cy = ssCurrentYear s
  let a = asAssets cy
  let a2 = simulateMarket mi a
  let pnl = marketPnl a2 a
  let cy' = cy{asAssets = a2,asRRSPMarketPnl = aRrsp pnl, asTFSAMarketPnl = aTfsa pnl}
  put s{ssCurrentYear = cy'}

endOfYear :: YearlyInput -> SState ()
endOfYear yi = do
  s <- get
  let nYear = ssYear s + 1
  let nAge = ssAge s + 1
  let s1 = initialSimulationState nYear nAge (asAssets $ ssCurrentYear s)
  let olds = ssPreviousYear s
  put s1{ssPreviousYear = (yi, ssCurrentYear s) : olds}
  

resultsToReport :: SimulationResults -> [[String]]
resultsToReport res =
  let yearHeader = 
        ["Année","Age","Fond insuffisant",
         "Revenu disponible","Revenu disponible cible",
         "Salaire","Dividende déterminé","Dividende non déterminé", "Impôt fédéral","Impôt provincial",
         "Contribution REER","Contribution CELI","Rendement REER","Rendement CELI","Valeur REER","Valeur CELI",
         "Taux de rendement"]
      finalYear = srYear res
      finalAge = srAge res
      yearData1 = snd $ foldl' yearData ((finalYear, finalAge), []) (srData res)
  in yearHeader : yearData1

yearData :: ((Int, Int), [[String]]) -> (YearlyInput, AssetsState) -> ((Int, Int), [[String]])
yearData ((year, age), xs) (mi, ss) =
  let y = year - 1
      a = age - 1
      x = serializeYear y a mi ss
  in ((y, a), x:xs)


serializeYear :: Int -> Int -> YearlyInput -> AssetsState -> [String]
serializeYear year age yi as =
  let income = fiIncome $ yiFinancialInput yi
      taxInfo = fiIncomeTaxInfo $ yiFinancialInput yi
      rrspContrib = asRRSPContrib as
      tfsaContrib = asTFSAContrib as
      taxReport = computeTax taxInfo $ TaxReportInput income rrspContrib
      revDispo = afterTaxIncome $ taxReport
      cible = fiNeedForSpending $ yiFinancialInput yi
      revDispoCible = afterTaxIncome $ computeTax taxInfo $ TaxReportInput (salary cible) 0
  in
    [show year,
    show age,
    if revDispo < revDispoCible then "VRAI" else "FAUX",
    show revDispo,
    show revDispoCible,
    show (iSalary income),
    show (iEligibleDividend income),
    show (iNonEligibleDividend income),
    show (trFedIncomeTax taxReport),
    show (trQcIncomeTax taxReport),
    show rrspContrib,
    show tfsaContrib,
    show (asRRSPMarketPnl as),
    show (asTFSAMarketPnl as),
    show (aRrsp $ asAssets as),
    show (aTfsa $ asAssets as),
    show (yiMarketInput yi)] 