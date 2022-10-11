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
  FinancialInput(..),
  SState,
  Strategy(..),
  doNothing,
  runSimulation,
  resultsToReport,
  SimulationState(..),
  AssetsState(..),
  ssAddToRrsp,
  ssAddToBank,
  ssAddToTfsa
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
  infoRrspContribRights :: Decimal, -- To compute RRSP contribution
  infoTfsaContribRights :: Decimal, -- To compute RRSP contribution
  infoYearlyData :: [YearlyInput]
} deriving (Show, Eq)

data YearlyInput = YearlyInput {
  yiFinancialInput :: FinancialInput, -- Things known to the investors
  yiMarketInput :: MarketInput -- Things unknown to the investors
} deriving (Show, Eq)

data FinancialInput = FinancialInput {
  fiIncome :: Income, -- Before income tax
  fiNeedForSpending :: DisposableIncome, -- Before income tax as a salary (no dividend)
  fiIncomeTaxInfo :: IncomeTaxInfo
} deriving (Show, Eq)

-- Everything that a strategy can have acces to and possibly modify
data SimulationState = SimulationState {
  ssYear :: Int,
  ssAge :: Int,
  ssAssetsState :: AssetsState,
  ssPreviousYears :: [(YearlyInput, AssetsState)]
} deriving (Show, Eq)

data AssetsState = AssetsState {
  asAssets :: Assets,
  asRRSPContrib :: Decimal,
  asRRSPMarketPnl :: Decimal,
  asRRSPContribRights :: Decimal,
  asTFSAContrib :: Decimal,
  asTFSAMarketPnl :: Decimal,
  asTFSAContribRights :: Decimal,
  asBankContrib :: Decimal
} deriving (Show, Eq)

ssAddToRrsp :: SimulationState -> Decimal -> SimulationState
ssAddToRrsp s x =
  let a = ssAssetsState s
      as = asAssets a
      as' = addToRrsp as x
      c = asRRSPContrib a
      c' = c + x
      a' = a{asAssets = as', asRRSPContrib = c'}
  in s{ssAssetsState = a'}

ssAddToTfsa :: SimulationState -> Decimal -> SimulationState
ssAddToTfsa s x =
  let a = ssAssetsState s
      as = asAssets a
      as' = addToTfsa as x
      c = asTFSAContrib a
      c' = c + x
      a' = a{asAssets = as', asTFSAContrib = c'}
  in s{ssAssetsState = a'}

ssAddToBank :: SimulationState -> Decimal -> SimulationState
ssAddToBank s x =
  let a = ssAssetsState s
      as = asAssets a
      as' = addToBank as x
      c = asBankContrib a
      c' = c + x
      a' = a{asAssets = as', asBankContrib = c'}
  in s{ssAssetsState = a'}

initialSimulationState :: Int -> Int -> Assets -> Decimal -> Decimal -> SimulationState
initialSimulationState year age assets rrsp tfsa = 
  SimulationState year age (AssetsState assets 0 0 rrsp 0 0 tfsa 0) []

type SState = State SimulationState

data Strategy = Strategy {
  sName :: String,
  -- Use the data available in the state to make sure we have enough for
  -- needForSpending, pay the taxes and then invest the surplus.
  sPayTaxesAndInvest :: FinancialInput -> SState ()
} 

doNothing :: Strategy
doNothing = Strategy "do nothing" (const $ pure ())

instance Monoid Strategy where
  mempty = doNothing

instance Semigroup Strategy where
  (Strategy n1 f1) <> (Strategy n2 f2) = Strategy (n1 ++ " >>> " ++ n2) (f1 >> f2) 

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
                                  (infoRrspContribRights info)
                                  (infoTfsaContribRights info)
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
      let ds = ssPreviousYears s
      pure $ SimulationResults year age ds
    alg (Cons yearlyInput next) = do
      sPayTaxesAndInvest strat (yiFinancialInput yearlyInput) 
      simulateMarket' (yiMarketInput yearlyInput)
      endOfYear yearlyInput
      next

simulateMarket' :: MarketInput -> SState ()
simulateMarket' mi = do
  s <- get
  let cy = ssAssetsState s
  let a = asAssets cy
  let a2 = simulateMarket mi a
  let pnl = marketPnl a a2
  let cy' = cy{asAssets = a2,asRRSPMarketPnl = aRrsp pnl, asTFSAMarketPnl = aTfsa pnl}
  put s{ssAssetsState = cy'}

endOfYear :: YearlyInput -> SState ()
endOfYear yi = do
  s <- get
  -- We get older
  let nYear = ssYear s + 1
  let nAge = ssAge s + 1
  -- Compute next RRSP contribution rights
  let fi = yiFinancialInput yi
  let mySalary = iSalary $ fiIncome $ fi
  let maxRRSP1 = maxRRSPContrib $ fiIncomeTaxInfo fi
  let maxRRSP2 = roundTo 2 $ mySalary *. (maxRRSPRate $ fiIncomeTaxInfo fi)
  let maxRRSP' = min maxRRSP1 maxRRSP2
      -- Previous balance, plus this year possible contribution minus the actual contribution
  let nRRSP = if nAge > (maxRRSPAge $ fiIncomeTaxInfo fi)
              then 0
              else (asRRSPContribRights $ ssAssetsState s) + maxRRSP' - (asRRSPContrib $ ssAssetsState s)
  -- Compute next Tfas contribution rights
  let maxTFSA = maxTFSAContrib $ fiIncomeTaxInfo fi
  let nTFSA = (asTFSAContribRights $ ssAssetsState s) + maxTFSA - (asTFSAContrib $ ssAssetsState s)

  -- Update state
  let s1 = initialSimulationState nYear nAge (asAssets $ ssAssetsState s) nRRSP nTFSA
  let olds = ssPreviousYears s
  put s1{ssPreviousYears = (yi, ssAssetsState s) : olds}
  

resultsToReport :: Maybe Char -> SimulationResults -> [[String]]
resultsToReport c res =
  let yearHeader = 
        ["Année","Age","Fond insuffisant",
         "Revenu disponible","Revenu disponible cible",
         "Salaire","Dividende déterminé","Dividende non déterminé", "Impôt fédéral","Impôt provincial",
         "Contribution REER","Rendement REER","Droits REER", "Valeur REER",
         "Contribution CELI","Rendement CELI","Droits CELI", "Valeur CELI",
         "Taux de rendement"]
      finalYear = srYear res
      finalAge = srAge res
      yearData1 = snd $ foldl' (yearData c) ((finalYear, finalAge), []) (srData res)
  in yearHeader : yearData1

yearData :: Maybe Char -> ((Int, Int), [[String]]) -> (YearlyInput, AssetsState) -> ((Int, Int), [[String]])
yearData c ((year, age), xs) (yi, as) =
  let x = serializeYear c year age yi as
      y = year - 1
      a = age - 1
  in ((y, a), x:xs)


serializeYear :: Maybe Char -> Int -> Int -> YearlyInput -> AssetsState -> [String]
serializeYear c year age yi as =
  let income = fiIncome $ yiFinancialInput yi
      taxInfo = fiIncomeTaxInfo $ yiFinancialInput yi
      rrspContrib = asRRSPContrib as
      tfsaContrib = asTFSAContrib as
      taxReport = computeTax taxInfo $ TaxReportInput income rrspContrib
      revDispo = afterTaxIncome taxReport - tfsaContrib
      cible = fiNeedForSpending $ yiFinancialInput yi
      revDispoCible = toAfterTax cible taxInfo
      showDecimal :: Decimal -> String
      showDecimal = case c of
                      Nothing -> show
                      (Just c') -> map (\x -> if x == '.' then c' else x) . show
  in
    [show year,
    show age,
    if revDispo < revDispoCible then "VRAI" else "FAUX",
    showDecimal revDispo,
    showDecimal revDispoCible,
    showDecimal (iSalary income),
    showDecimal (iEligibleDividend income),
    showDecimal (iNonEligibleDividend income),
    showDecimal (trFedIncomeTax taxReport),
    showDecimal (trQcIncomeTax taxReport),
    showDecimal rrspContrib,
    showDecimal (asRRSPMarketPnl as),
    showDecimal (asRRSPContribRights as),
    showDecimal (aRrsp $ asAssets as),
    showDecimal tfsaContrib,
    showDecimal (asTFSAMarketPnl as),
    showDecimal (asTFSAContribRights as),
    showDecimal (aTfsa $ asAssets as),
    showDecimal (fromRational $ yiMarketInput yi :: Decimal)] 