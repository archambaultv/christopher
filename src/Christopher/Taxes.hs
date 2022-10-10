{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, TemplateHaskell, TypeFamilies #-}
-- |
-- Module      :  Christopher.Taxes
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines what a financial planning simulation is and how to run it

module Christopher.Taxes
(
  IncomeTaxInfo(..),
  TaxBrackets(..),
  TaxBracketsF(..),
  DividendTax(..),
  PersonnalAmnt(..),
  personnalAmount,
  FederalIncomeTax(..),
  QuebecIncomeTax(..),
  Income(..),
  salary,
  computeTax,
  computeFedTax,
  computeQcTax,
  TaxReport(..),
  TaxReportInput(..),
  afterTaxIncome,
  incomeTaxTable
)
where

import Data.Decimal
import Data.Functor.Foldable
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Christopher.Markets (Rate)

data IncomeTaxInfo = Taxes {
  fedTaxes :: FederalIncomeTax,
  qcTaxes :: QuebecIncomeTax
} deriving (Show, Eq)

data FederalIncomeTax = FederalIncomeTax{
  fedTaxBrackets :: TaxBrackets,
  fedBasicPersonnalAmnt :: PersonnalAmnt,
  fedQuebecAbatement :: Rate,
  fedCreditMultiplier :: Rate,
  fedDividendTax :: DividendTax
} deriving (Show, Eq)

data DividendTax = DividendTax {
  eligibleCreditRate :: Rate,
  eligibleDivMultiplier :: Rate,
  nonEligibleCreditRate :: Rate,
  nonEligibleDivMultiplier :: Rate
} deriving (Show, Eq)

data PersonnalAmnt = ConstPersonnalAmnt Decimal
                   | LinearPersonnalAmnt Decimal -- Low income amount
                                         Decimal -- Low income threshold
                                         Decimal -- High income min amount
                                         Decimal -- High income divider
  deriving (Show, Eq)

personnalAmount :: PersonnalAmnt -> Decimal -> Decimal
personnalAmount (ConstPersonnalAmnt x) _ = x
personnalAmount (LinearPersonnalAmnt bAmnt bInc hiAmnt hiDiv) netInc =
  if netInc < bInc
  then bAmnt
  else 
    let netDiff = netInc - bInc
        diffMiddle = hiDiv - netDiff
        percent = diffMiddle / hiDiv -- No rounding
        amnt = roundTo 2 $ (bAmnt - hiAmnt) * percent
    in if diffMiddle <= 0 then hiAmnt else hiAmnt + amnt

data QuebecIncomeTax = QuebecIncomeTax{
  qcTaxBrackets :: TaxBrackets,
  qcBasicPersonnalAmnt :: PersonnalAmnt,
  qcCreditMultiplier :: Rate,
  qcDividendTax :: DividendTax,
  qcDeductionForWorkersRate :: Rate,
  qcDeductionForWorkersMax :: Decimal
} deriving (Show, Eq)

-- Brackets limit must be in increasing order
data TaxBrackets
  = TopTaxBracket Rate -- Anything above
  | TaxBracket Decimal Rate TaxBrackets -- Applies to any amount below or equal to this one
 deriving (Show, Eq)

makeBaseFunctor ''TaxBrackets

data Income = Income {
  iSalary :: Decimal,
  iEligibleDividend :: Decimal, -- Determiné
  iNonEligibleDividend :: Decimal
} deriving (Show, Eq)

salary :: Decimal -> Income
salary x = Income x 0 0

totalIncome :: Income -> Decimal
totalIncome income = iSalary income 
                    + iEligibleDividend income
                    + iNonEligibleDividend income

-- Dividend are multiplied by their multiplier
totalTaxIncome ::  DividendTax -> Income -> Decimal
totalTaxIncome t income = iSalary income + d1' + d2'
  where d1' = roundTo 2 $ (iEligibleDividend income) *. (1 + eligibleDivMultiplier t)
        d2' = roundTo 2 $ (iNonEligibleDividend income) *. (1 + nonEligibleDivMultiplier t)

dividendCredit :: DividendTax -> Income -> (Decimal, Decimal)
dividendCredit t income = (eligibleCredit, nonEligibleCredit)
  where
      eligibleCredit = roundTo 2 
                     $ (iEligibleDividend income) 
                     *. (1 + eligibleDivMultiplier t) 
                     *. (eligibleCreditRate t)
      nonEligibleCredit = roundTo 2 
                        $ (iNonEligibleDividend income) 
                        *. (1 + nonEligibleDivMultiplier t) 
                        *. (nonEligibleCreditRate t)

personalCredit :: PersonnalAmnt -> Decimal -> Rate -> Decimal
personalCredit t i r = roundTo 2 $ personnalAmount t i *. r

applyBrackets :: TaxBrackets -> Decimal -> Decimal
applyBrackets t x = roundTo 2 $ cata alg t (0,0)
  where
    alg :: TaxBracketsF ((Decimal, Decimal) -> Decimal) -> ((Decimal, Decimal) -> Decimal) 
    -- We have reach the top bracket
    alg (TopTaxBracketF rate) (acc, lowerLimit) = acc + (x - lowerLimit) *. rate

    -- We have not reach the first bracket
    alg (TaxBracketF limit rate upperBrackets) (acc, lowerLimit) =
      if x < limit
      then -- This bracket if the final stop
           acc + (x - lowerLimit) *. rate
      else  
           -- Compute this bracket and move on to the next
        let acc' = acc + (limit - lowerLimit) *. rate
        in upperBrackets (acc', limit)

data TaxReport = TaxReport {
  trTaxReportInput :: TaxReportInput,
  trFedIncomeTax :: Decimal,
  trQcIncomeTax :: Decimal
} deriving (Show, Eq)


data TaxReportInput = TaxReportInput {
  tiIncome :: Income,
  tiRRSPContrib :: Decimal
} deriving (Show, Eq)

afterTaxIncome :: TaxReport -> Decimal
afterTaxIncome (TaxReport r t1 t2) = totalIncome (tiIncome r) - t1 - t2 - (tiRRSPContrib r)

-- Returns the after tax amount and the taxes paid
computeTax :: IncomeTaxInfo -> TaxReportInput -> TaxReport
computeTax taxes r =
  let tFed = computeFedTax (fedTaxes taxes) r
      tQc = computeQcTax (qcTaxes taxes) r
  in TaxReport r tFed tQc


computeFedTax :: FederalIncomeTax -> TaxReportInput -> Decimal
computeFedTax fedTax r =
  let -- Step 2, compute total income
      totalIncome' = totalTaxIncome (fedDividendTax fedTax) (tiIncome r)
      -- Step 3, net income
      netIncome = totalIncome' - (tiRRSPContrib r)
      -- Step 4, taxable income
      taxableIncome = netIncome
      -- Step 5.A Federal gross income tax 
      tax = applyBrackets (fedTaxBrackets fedTax) taxableIncome
      -- Step 5.B non refundable tax credit
      nonRefundableCr = personalCredit (fedBasicPersonnalAmnt fedTax) taxableIncome (fedCreditMultiplier fedTax) 
      -- Step 5.C federal net income tax
      (d1, d2) = dividendCredit (fedDividendTax fedTax) (tiIncome r)
      divCredit = d1 + d2
      netTax = max 0 $ tax - nonRefundableCr - divCredit 
      -- Step 6, Provincial tax
      -- Step 7, Amount due
      amntDueTax = roundTo 2 $ netTax  *. (1 - fedQuebecAbatement fedTax)
  in amntDueTax

  where

computeQcTax :: QuebecIncomeTax -> TaxReportInput -> Decimal
computeQcTax qcTax r =
  let -- Step 1, compute total income
      totalIncome' = totalTaxIncome (qcDividendTax qcTax) (tiIncome r)
      -- Step 2, net income
      -- workerCredit = min (qcDeductionForWorkersMax qcTax) 
      --              $ (iSalary r) *. (qcDeductionForWorkersRate qcTax)
      --  Don't compute worker's credit if we don't compute QPP, RQAP and other deduction
      netIncome =  totalIncome' - (tiRRSPContrib r)
      -- Step 3, taxable income
      taxableIncome = netIncome
      -- Step 4, non refundable tax credit
      pc = personalCredit (qcBasicPersonnalAmnt qcTax) taxableIncome (qcCreditMultiplier qcTax)
      -- Step 5, income taxes
      tax = applyBrackets (qcTaxBrackets qcTax) taxableIncome
      tax2 = tax - pc -- line 413
      (d1, d2) = dividendCredit (qcDividendTax qcTax) (tiIncome r)
      tax3 = max 0 $ tax2 - d1 - d2 -- line 430
      
  in tax3

incomeTaxTable :: IncomeTaxInfo -> [TaxReport]
incomeTaxTable info =
  let incomes = [x * 1000 | x <- [10..80]] -- Up to 80K
              ++ [80000 + x * 5000 | x <- [1..24]] -- Up to 200K
              ++ [200000 + x * 10000 | x <- [1..20]] -- Up to 400K
  in map (\x -> computeTax info $ TaxReportInput (salary x) 0) incomes