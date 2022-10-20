{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module      :  Christopher.Taxes.PersonnalTax
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines the personnal tax

module Christopher.Taxes.PersonnalTax
(
  PersonnalTax(..),
  FederalPersonnalTax(..),
  QuebecPersonnalTax(..),
  LinearPersonnalAmnt(..),
  personnalAmount,
  TaxBrackets(..),
  sortTaxBrackets,
  taxBrackets,
  DividendTax(..),
  DisposableIncome(..),
  toAfterTax,
  PersonnalTaxReport(..),
  PersonnalTaxInput(..),
  computePersonnalTax,
  disposableIncome,
  SocialChargesRates(..),
  SocialCharges(..),
  totalSocialCharges,
  socialCharges,
)
where

import GHC.Generics
import Data.List (sortOn, group, sort)
import Data.Functor.Foldable
import Data.Aeson (ToJSON(..), FromJSON(..), genericToEncoding, 
                   genericToJSON, genericParseJSON)
import Christopher.Amount
import Christopher.Taxes.Income
import Christopher.Internal.LabelModifier

data PersonnalTax = PersonnalTax {
  ptFederalPersonnalTax :: FederalPersonnalTax,
  ptQuebecPersonnalTax :: QuebecPersonnalTax
} deriving (Show, Eq, Generic)

-- Remove duplicates and sorts them
taxBrackets :: PersonnalTax -> [Amount]
taxBrackets info = 
  let f = map tbAbove $ tbBrackets $ fedTaxBrackets $ ptFederalPersonnalTax info
      q = map tbAbove $ tbBrackets $ qcTaxBrackets $ ptQuebecPersonnalTax info
  in map head $ group $ sort (f ++ q)

instance ToJSON PersonnalTax where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON PersonnalTax where
  parseJSON = genericParseJSON jsonOptions

data FederalPersonnalTax = FederalPersonnalTax{
  fedTaxBrackets :: TaxBrackets,
  fedBasicPersonnalAmount :: LinearPersonnalAmnt,
  fedQuebecAbatement :: Rate,
  fedNonRefundableTaxCreditsRate :: Rate,
  fedDividendTax :: DividendTax
} deriving (Show, Eq, Generic)

instance ToJSON FederalPersonnalTax where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON FederalPersonnalTax where
  parseJSON = genericParseJSON jsonOptions

data QuebecPersonnalTax = QuebecPersonnalTax{
  qcTaxBrackets :: TaxBrackets,
  qcBasicPersonnalAmount :: Amount,
  qcNonRefundableTaxCreditsRate :: Rate,
  qcDividendTax :: DividendTax
} deriving (Show, Eq, Generic)

instance ToJSON QuebecPersonnalTax where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON QuebecPersonnalTax where
  parseJSON = genericParseJSON jsonOptions

data DividendTax = DividendTax {
  dtEligibleCreditRate :: Rate,
  dtEligibleMultiplier :: Rate,
  dtNonEligibleCreditRate :: Rate,
  dtNonEligibleMultiplier :: Rate
} deriving (Show, Eq, Generic)

instance ToJSON DividendTax where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON DividendTax where
  parseJSON = genericParseJSON jsonOptions

data LinearPersonnalAmnt  = LinearPersonnalAmnt {
  lpaMaximumAmount :: Amount,
  lpaMaximumThreshold :: Amount,
  lpaMinimumAmount :: Amount,
  lpaMinimumThreshold :: Amount
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
       else let slope = (minAmnt - maxAmnt) / (minLimit - maxLimit)
                diff = netInc - maxLimit
            in roundTo 2 $ maxAmnt + slope * diff

-- Brackets limit must be in increasing order
data TaxBrackets = TaxBrackets {
  tbBaseRate :: Rate,
  tbBrackets :: [TaxBracket]
} deriving (Eq, Show, Generic)

data TaxBracket = TaxBracket {
  tbAbove :: Amount,
  tbRate :: Rate
}
 deriving (Show, Eq, Generic)

instance ToJSON TaxBrackets where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON TaxBrackets where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TaxBracket where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON TaxBracket where
  parseJSON = genericParseJSON jsonOptions

data DisposableIncome 
  = DIAfterTax Amount 
  | DIBeforeTax Amount -- As a salary without any dividend
  deriving (Show, Eq)

toAfterTax :: DisposableIncome -> PersonnalTax -> Amount
toAfterTax target taxInfo =
  case target of
    DIBeforeTax x -> disposableIncome 
                      $ computePersonnalTax taxInfo 
                      $ PersonnalTaxInput (salary x) 0
    DIAfterTax x -> x

totalIncome :: Income -> Amount
totalIncome income = iSalary income 
                    + iEligibleDividend income
                    + iNonEligibleDividend income

-- Dividend are multiplied by their multiplier
totalTaxIncome ::  DividendTax -> Income -> Amount
totalTaxIncome t income = iSalary income + d1' + d2'
  where d1' = roundTo 2 $ (iEligibleDividend income) *. (1 + dtEligibleMultiplier t)
        d2' = roundTo 2 $ (iNonEligibleDividend income) *. (1 + dtNonEligibleMultiplier t)

dividendCredit :: DividendTax -> Income -> (Amount, Amount)
dividendCredit t income = (eligibleCredit, nonEligibleCredit)
  where
      eligibleCredit = roundTo 2 
                     $ (iEligibleDividend income) 
                     *. (1 + dtEligibleMultiplier t) 
                     *. (dtEligibleCreditRate t)
      nonEligibleCredit = roundTo 2 
                        $ (iNonEligibleDividend income) 
                        *. (1 + dtNonEligibleMultiplier t) 
                        *. (dtNonEligibleCreditRate t)

applyBrackets :: TaxBrackets -> Amount -> Amount
applyBrackets (TaxBrackets baseRate brackets) x = para alg (TaxBracket 0 baseRate : brackets) 0
  where
    alg :: ListF TaxBracket ([TaxBracket], Amount -> Amount) -> (Amount -> Amount) 
    alg Nil acc = acc
    alg (Cons (TaxBracket limit rate) ([], _)) acc = 
      roundTo 2 $ acc + (x - limit) *. rate
    alg (Cons (TaxBracket limit rate) ((TaxBracket nextLimit _ : _), next)) acc =
      if x <= nextLimit
      then -- This bracket if the final stop
           roundTo 2 $ acc + (x - limit) *. rate
      else  
           -- Compute this bracket and move on to the next
        let acc' = roundTo 2 $ acc + (nextLimit - limit) *. rate
        in next acc'

data PersonnalTaxReport = PersonnalTaxReport {
  trTaxReportInput :: PersonnalTaxInput,
  trFedPersonnalTax :: Amount,
  trQcPersonnalTax :: Amount
} deriving (Show, Eq)

data PersonnalTaxInput = PersonnalTaxInput {
  tiIncome :: Income,
  tiRRSPContrib :: Amount
} deriving (Show, Eq)

disposableIncome :: PersonnalTaxReport -> Amount
disposableIncome (PersonnalTaxReport r t1 t2) = totalIncome (tiIncome r) - t1 - t2 - (tiRRSPContrib r)

-- Returns the after tax amount and the taxes paid
computePersonnalTax :: PersonnalTax -> PersonnalTaxInput -> PersonnalTaxReport
computePersonnalTax taxes r =
  let tFed = computeFedTax (ptFederalPersonnalTax taxes) r
      tQc = computeQcTax (ptQuebecPersonnalTax taxes) r
  in PersonnalTaxReport r tFed tQc


computeFedTax :: FederalPersonnalTax -> PersonnalTaxInput -> Amount
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
                      $ personnalAmount (fedBasicPersonnalAmount fedTax) taxableIncome 
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

computeQcTax :: QuebecPersonnalTax -> PersonnalTaxInput -> Amount
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
      pc = (qcBasicPersonnalAmount qcTax) 
         *. (qcNonRefundableTaxCreditsRate qcTax)
      -- Step 5, income taxes
      tax = applyBrackets (qcTaxBrackets qcTax) taxableIncome
      tax2 = tax - pc -- line 413
      (d1, d2) = dividendCredit (qcDividendTax qcTax) (tiIncome r)
      tax3 = max 0 $ tax2 - d1 - d2 -- line 430
      
  in tax3

sortTaxBrackets' :: TaxBrackets -> TaxBrackets
sortTaxBrackets' (TaxBrackets x xs) = TaxBrackets x (sortOn tbAbove xs)

sortTaxBrackets :: PersonnalTax -> PersonnalTax
sortTaxBrackets (PersonnalTax f q) =
  let ftb' = sortTaxBrackets' $ fedTaxBrackets f
      qtb' = sortTaxBrackets' $ qcTaxBrackets q
  in PersonnalTax f{fedTaxBrackets = ftb'} q{qcTaxBrackets = qtb'}

data SocialChargesRates = SocialChargesRates {
  scrRRQRate :: Rate,
  scrRRQMaxSalary :: Amount,
  scrRQAPRate :: Rate,
  scrRQAPMaxSalary :: Amount,
  scrFSSRate :: Rate
} deriving (Show, Eq, Generic)

instance ToJSON SocialChargesRates where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON SocialChargesRates where
  parseJSON = genericParseJSON jsonOptions

data SocialCharges = SocialCharges {
  scRRQ :: Amount,
  scRQAP :: Amount,
  scFSS :: Amount
} deriving (Show, Eq)

totalSocialCharges :: SocialCharges -> Amount
totalSocialCharges (SocialCharges a b c) = a + b + c

socialCharges :: SocialChargesRates -> Salary -> SocialCharges
socialCharges sc s = 
  let rrq = roundTo 2 $ (min (scrRRQMaxSalary sc) s) *. (scrRRQRate sc)
      rqap = roundTo 2 $ (min (scrRQAPMaxSalary sc) s) *. (scrRQAPRate sc)
      fss = roundTo 2 $ s *. scrFSSRate sc
  in SocialCharges rrq rqap fss