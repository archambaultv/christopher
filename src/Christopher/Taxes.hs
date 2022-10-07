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
  IncomeTax(..),
  FederalIncomeTax(..),
  QuebecIncomeTax(..),
  Income(..),
  computeTax,
  computeFedTax,
  computeQcTax
)
where

import Data.Decimal
import Data.Functor.Foldable
import Christopher.Markets (Rate)

data IncomeTaxInfo = Taxes {
  fedTaxes :: FederalIncomeTax,
  qcTaxes :: QuebecIncomeTax
}

data TaxBrackets
  = FirstTaxBracket Rate 
  | TaxBracket Decimal Rate TaxBrackets -- Applies to any amount above or equal to this one
 deriving (Show, Eq)

data FederalIncomeTax = FederalIncomeTax{
  fedTaxBrackets :: TaxBrackets,
  fedBasicPersonnalAmnt :: Decimal,
  fedQuebecAbatement :: Rate,
  fedCreditMultiplier :: Rate,
  fedEligibleCreditRate :: Rate,
  fedEligibleDivMultiplier :: Rate,
  fedNonEligibleCreditRate :: Rate,
  fedNonEligibleDivMultiplier :: Rate
}

data QuebecIncomeTax = QuebecIncomeTax{
  qcTaxBrackets :: TaxBrackets,
  qcBasicPersonnalAmnt :: Decimal,
  qcCreditMultiplier :: Rate,
  qcEligibleCreditRate :: Rate,
  qcEligibleDivMultiplier :: Rate,
  qcNonEligibleCreditRate :: Rate,
  qcNonEligibleDivMultiplier :: Rate
}

-- Things that are common to both Canada and Quebec, so we can share
-- some code
class IncomeTax a where
  taxBrackets :: a -> TaxBrackets
  basicPersonnalAmnt :: a -> Decimal
  creditMultiplier :: a -> Rate
  eligibleCreditRate :: a -> Rate
  eligibleDivMultiplier :: a -> Rate
  nonEligibleCreditRate :: a -> Rate
  nonEligibleDivMultiplier :: a -> Rate

instance IncomeTax QuebecIncomeTax where
  taxBrackets = qcTaxBrackets
  basicPersonnalAmnt = qcBasicPersonnalAmnt
  creditMultiplier = qcCreditMultiplier
  eligibleCreditRate = qcEligibleCreditRate
  eligibleDivMultiplier = qcEligibleDivMultiplier
  nonEligibleCreditRate = qcNonEligibleCreditRate
  nonEligibleDivMultiplier = qcNonEligibleDivMultiplier

instance IncomeTax FederalIncomeTax where
  taxBrackets = fedTaxBrackets
  basicPersonnalAmnt = fedBasicPersonnalAmnt
  creditMultiplier = fedCreditMultiplier
  eligibleCreditRate = fedEligibleCreditRate
  eligibleDivMultiplier = fedEligibleDivMultiplier
  nonEligibleCreditRate = fedNonEligibleCreditRate
  nonEligibleDivMultiplier = fedNonEligibleDivMultiplier

data Income = Income {
  iSalary :: Decimal,
  iEligibleDividend :: Decimal, -- Determiné
  iNonEligibleDividend :: Decimal
}

totalIncome :: Income -> Decimal
totalIncome income = iSalary income 
                    + iEligibleDividend income
                    + iNonEligibleDividend income

-- Dividend are multiplied by their multiplier
totalTaxIncome :: (IncomeTax a) => a -> Income -> Decimal
totalTaxIncome t income = iSalary income + d1' + d2'
  where d1' = roundTo 2 $ (iEligibleDividend income) *. (1 + eligibleDivMultiplier t)
        d2' = roundTo 2 $ (iNonEligibleDividend income) *. (1 + nonEligibleDivMultiplier t)

dividendCredit :: (IncomeTax a) => a -> Income -> (Decimal, Decimal)
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

personalCredit :: (IncomeTax a) => a -> Decimal
personalCredit t = roundTo 2 $ basicPersonnalAmnt t *. creditMultiplier t

applyBrackets :: (IncomeTax a) => a -> Decimal -> Decimal
applyBrackets _ _ = 0

-- Returns the after tax amount and the taxes paid
computeTax :: IncomeTaxInfo -> Income -> (Decimal, Decimal)
computeTax taxes r =
  let total = totalIncome r
      tFed = computeFedTax (fedTaxes taxes) r
      tQc = computeQcTax (qcTaxes taxes) r
      ts = tFed + tQc
  in (total - ts, ts)


computeFedTax :: FederalIncomeTax -> Income -> Decimal
computeFedTax fedTax r =
  let total = totalTaxIncome fedTax r
      tax = applyBrackets fedTax total
      (d1, d2) = dividendCredit fedTax r
      pc = personalCredit fedTax 
  in tax - d1 - d2 - pc

  where

computeQcTax :: QuebecIncomeTax -> Income -> Decimal
computeQcTax qcTax r =
  let total = totalTaxIncome qcTax r
      tax = applyBrackets qcTax total
      (d1, d2) = dividendCredit qcTax r
      pc = personalCredit qcTax
  in tax - d1 - d2 - pc