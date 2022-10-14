{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable, TemplateHaskell, TypeFamilies #-}
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
  LinearPersonnalAmnt(..),
  personnalAmount,
  FederalIncomeTax(..),
  QuebecIncomeTax(..),
  Income(..),
  DisposableIncome(..),
  toAfterTax,
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

import GHC.Generics
import Data.Functor.Foldable
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Aeson (ToJSON(..), FromJSON(..), genericToEncoding, 
                   genericToJSON, genericParseJSON)
import Christopher.Amount
import Christopher.Utils

data IncomeTaxInfo = Taxes {
  fedTaxes :: FederalIncomeTax,
  qcTaxes :: QuebecIncomeTax
} deriving (Show, Eq, Generic)

instance ToJSON IncomeTaxInfo where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON IncomeTaxInfo where
  parseJSON = genericParseJSON jsonOptions

data FederalIncomeTax = FederalIncomeTax{
  fedTaxBrackets :: TaxBrackets,
  fedBasicPersonnalAmnt :: LinearPersonnalAmnt,
  fedQuebecAbatement :: Rate,
  fedNonRefundableTaxCreditsRate :: Rate,
  fedDividendTax :: DividendTax
} deriving (Show, Eq, Generic)

instance ToJSON FederalIncomeTax where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON FederalIncomeTax where
  parseJSON = genericParseJSON jsonOptions

data QuebecIncomeTax = QuebecIncomeTax{
  qcTaxBrackets :: TaxBrackets,
  qcBasicPersonnalAmnt :: Amount,
  qcNonRefundableTaxCreditsRate :: Rate,
  qcDividendTax :: DividendTax,
  qcDeductionForWorkersRate :: Rate,
  qcDeductionForWorkersMax :: Amount
} deriving (Show, Eq, Generic)

instance ToJSON QuebecIncomeTax where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON QuebecIncomeTax where
  parseJSON = genericParseJSON jsonOptions

data DividendTax = DividendTax {
  eligibleCreditRate :: Rate,
  eligibleDivMultiplier :: Rate,
  nonEligibleCreditRate :: Rate,
  nonEligibleDivMultiplier :: Rate
} deriving (Show, Eq, Generic)

instance ToJSON DividendTax where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON DividendTax where
  parseJSON = genericParseJSON jsonOptions

data LinearPersonnalAmnt  = LinearPersonnalAmnt {
  lpaMaximumAmount :: Amount,
  lpaMaximumThresold :: Amount,
  lpaMinimumAmount :: Amount,
  lpaMinimumThresold :: Amount
}
  deriving (Show, Eq, Generic)

instance ToJSON LinearPersonnalAmnt where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON LinearPersonnalAmnt where
  parseJSON = genericParseJSON jsonOptions

personnalAmount :: LinearPersonnalAmnt -> Amount -> Amount
personnalAmount (LinearPersonnalAmnt maxAmnt maxLimit minAmnt minLimit) netInc =
  if netInc <= maxLimit
  then maxAmnt
  else if netInc >= minLimit
       then minAmnt
       else let span' = maxLimit - minLimit
                diff = maxLimit - netInc
                percent = diff / span' -- No rounding
                amnt = roundTo 2 $ (maxAmnt - minAmnt) * percent
            in minAmnt + amnt

-- Brackets limit must be in increasing order
data TaxBrackets
  = TopTaxBracket Rate -- Anything above
  | TaxBracket Amount Rate TaxBrackets -- Applies to any amount below or equal to this one
 deriving (Show, Eq, Generic)

makeBaseFunctor ''TaxBrackets

data TaxBracket = 

instance ToJSON TaxBrackets where
  toJSON (TopTaxBracket r) =
    Array $ V.Vector (object ["top bracket rate" .= r])
  toJSON (TaxBracket amnt r xs) = 
    object $ ["less than" .= amnt, "rate" .= r]

  toEncoding (TopTaxBracket r) = 
    pairs $ "top bracket rate" .= r
  toEncoding (TaxBracket amnt r xs) = 
    pairs $ "less than" .= amnt <> "rate" .= r

instance FromJSON TaxBrackets where
  parseJSON = \v -> fmap tbFromList (parseJSON v)

data Income = Income {
  iSalary :: Amount,
  iEligibleDividend :: Amount, -- Determiné
  iNonEligibleDividend :: Amount
} deriving (Show, Eq, Generic)

instance ToJSON Income where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Income where
  parseJSON = genericParseJSON jsonOptions

instance Semigroup Income where
  i1 <> i2 = Income (iSalary i1 + iSalary i2)
                    (iEligibleDividend i1 + iEligibleDividend i2)
                    (iNonEligibleDividend i1 + iNonEligibleDividend i2)

instance Monoid Income where
  mempty = Income 0 0 0

data DisposableIncome 
  = DIAfterTax Amount 
  | DIBeforeTax Amount -- As a salary without any dividend
  deriving (Show, Eq)

toAfterTax :: DisposableIncome -> IncomeTaxInfo -> Amount
toAfterTax target taxInfo =
  case target of
    DIBeforeTax x -> afterTaxIncome 
                      $ computeTax taxInfo 
                      $ TaxReportInput (salary x) 0
    DIAfterTax x -> x

salary :: Amount -> Income
salary x = Income x 0 0

totalIncome :: Income -> Amount
totalIncome income = iSalary income 
                    + iEligibleDividend income
                    + iNonEligibleDividend income

-- Dividend are multiplied by their multiplier
totalTaxIncome ::  DividendTax -> Income -> Amount
totalTaxIncome t income = iSalary income + d1' + d2'
  where d1' = roundTo 2 $ (iEligibleDividend income) *. (1 + eligibleDivMultiplier t)
        d2' = roundTo 2 $ (iNonEligibleDividend income) *. (1 + nonEligibleDivMultiplier t)

dividendCredit :: DividendTax -> Income -> (Amount, Amount)
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

applyBrackets :: TaxBrackets -> Amount -> Amount
applyBrackets t x = roundTo 2 $ cata alg t (0,0)
  where
    alg :: TaxBracketsF ((Amount, Amount) -> Amount) -> ((Amount, Amount) -> Amount) 
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
  trFedIncomeTax :: Amount,
  trQcIncomeTax :: Amount
} deriving (Show, Eq)


data TaxReportInput = TaxReportInput {
  tiIncome :: Income,
  tiRRSPContrib :: Amount
} deriving (Show, Eq)

afterTaxIncome :: TaxReport -> Amount
afterTaxIncome (TaxReport r t1 t2) = totalIncome (tiIncome r) - t1 - t2 - (tiRRSPContrib r)

-- Returns the after tax amount and the taxes paid
computeTax :: IncomeTaxInfo -> TaxReportInput -> TaxReport
computeTax taxes r =
  let tFed = computeFedTax (fedTaxes taxes) r
      tQc = computeQcTax (qcTaxes taxes) r
  in TaxReport r tFed tQc


computeFedTax :: FederalIncomeTax -> TaxReportInput -> Amount
computeFedTax fedTax r =
  let -- Step 2, compute total income
      totalIncome' = totalTaxIncome (fedDividendTax fedTax) (tiIncome r)
      -- Step 3, net income
      netIncome = max 0 $ totalIncome' - (tiRRSPContrib r)
      -- Step 4, taxable income
      taxableIncome = netIncome
      -- Step 5.A Federal gross income tax 
      tax = applyBrackets (fedTaxBrackets fedTax) taxableIncome
      -- Step 5.B non refundable tax credit
      nonRefundableCr = roundTo 2 
                      $ personnalAmount (fedBasicPersonnalAmnt fedTax) taxableIncome 
                      *. (fedNonRefundableTaxCreditsRate fedTax) 
      -- Step 5.C federal net income tax
      (d1, d2) = dividendCredit (fedDividendTax fedTax) (tiIncome r)
      divCredit = d1 + d2
      netTax = max 0 $ tax - nonRefundableCr - divCredit 
      -- Step 6, Provincial tax
      -- Step 7, Amount due
      amntDueTax = roundTo 2 $ netTax  *. (1 - fedQuebecAbatement fedTax)
  in amntDueTax

  where

computeQcTax :: QuebecIncomeTax -> TaxReportInput -> Amount
computeQcTax qcTax r =
  let -- Step 1, compute total income
      totalIncome' = totalTaxIncome (qcDividendTax qcTax) (tiIncome r)
      -- Step 2, net income
      -- workerCredit = min (qcDeductionForWorkersMax qcTax) 
      --              $ (iSalary r) *. (qcDeductionForWorkersRate qcTax)
      --  Don't compute worker's credit if we don't compute QPP, RQAP and other deduction
      netIncome =  max 0 $ totalIncome' - (tiRRSPContrib r)
      -- Step 3, taxable income
      taxableIncome = netIncome
      -- Step 4, non refundable tax credit
      pc = (qcBasicPersonnalAmnt qcTax) 
         *. (qcNonRefundableTaxCreditsRate qcTax)
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