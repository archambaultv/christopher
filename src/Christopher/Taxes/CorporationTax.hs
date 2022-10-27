{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module      :  Christopher.Taxes.CorporationTax
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines the corporation tax for small business. This
-- is the small rate tax.

module Christopher.Taxes.CorporationTax
(
  CorporationTax(..),
  CorporationRates(..),
  CorporationTaxReport(..),
  CorporationTaxInput(..),
  computeCorporationTax,
  disposableEarning,
  activeEarningOnly,
  salaryAndCorporationSocialCharges
)
where

import GHC.Generics
import Data.Aeson (ToJSON(..), FromJSON(..), genericToEncoding, 
                   genericToJSON, genericParseJSON)
import Christopher.Amount
import Christopher.Internal.LabelModifier
import Christopher.Taxes.Income
import Christopher.Taxes.PersonnalTax

data CorporationTax = CorporationTax {
  ctFederalRates :: CorporationRates, -- Only one taxe rate (the small one)
  ctQuebecRates :: CorporationRates,
  ctSocialChargesRates :: SocialChargesRates,
  ctRDTOHRate :: Rate, -- Refundable dividend tax on hand (IMRTD in French)
  ctDividendRDTOHRate :: Rate
} deriving (Show, Eq, Generic)

instance ToJSON CorporationTax where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON CorporationTax where
  parseJSON = genericParseJSON jsonOptions

data CorporationTaxReport = CorporationTaxReport {
  ctTaxReportInput :: CorporationTaxInput,
  ctFedCorporationTax :: Amount,
  ctQcCorporationTax :: Amount,
  ctRDTOH :: Amount,
  ctCapitalDividendAccount :: Amount
} deriving (Show, Eq)

data CorporationTaxInput = CorporationTaxInput {
  ctiActiveEarning :: Amount,
  ctiPassiveCapitalGain :: Amount, -- All the gains must be passed, so we can compute CDA
  ctiPassiveDividendEarning :: Amount,
  ctiPassiveOtherEarning :: Amount,
  ctiRDTOH :: Amount,
  ctiCapitalDividendAccount :: Amount
} deriving (Show, Eq)

activeEarningOnly :: Amount -> CorporationTaxInput
activeEarningOnly x = CorporationTaxInput x mempty mempty mempty mempty mempty 

computeCorporationTax :: CorporationTax -> CorporationTaxInput -> CorporationTaxReport
computeCorporationTax (CorporationTax fed qc _ rDTOHRate dividendRDTOHRate) 
                    x@(CorporationTaxInput active capital divi passive rdtoh cda) = 
  let activeTax corpo =
        let small = min active (crActiveEarningSmallRateLimit corpo)
            above = max mempty (active -. (crActiveEarningSmallRateLimit corpo))
        in roundAwayFromZero
           $ small *. (crActiveEarningSmallRate corpo)
           + above *. (ctActiveEarningRate corpo)
      passiveTax corpo = roundAwayFromZero
                       $ passive *. (ctPassiveEarningRate corpo)
      divTax corpo = roundAwayFromZero
                   $ divi *. (ctPassiveDividendRate corpo)
      gainTax corpo = roundAwayFromZero
                    $ capital *. (0.5 * ctPassiveEarningRate corpo)
      halfCapital = roundAwayFromZero (capital *. 0.5)

  in CorporationTaxReport x 
     (activeTax fed +. passiveTax fed +. divTax fed +. gainTax fed)
     (activeTax qc +. passiveTax qc +. divTax qc +. gainTax fed)
     (rdtoh 
      +. roundAwayFromZero (halfCapital *. rDTOHRate)
      +. roundAwayFromZero (passive *. rDTOHRate)
      +. roundAwayFromZero (divi *. dividendRDTOHRate))
     (cda +. halfCapital)

disposableEarning :: CorporationTaxReport -> Amount
disposableEarning tr = 
  let x = ctTaxReportInput tr
      r = ctiActiveEarning x
        +. ctiPassiveCapitalGain x
        +. ctiPassiveDividendEarning x
        +. ctiPassiveOtherEarning x
  in r -. ctFedCorporationTax tr -. ctQcCorporationTax tr

data CorporationRates = CorporationRates {
  crActiveEarningSmallRate :: Rate,
  crActiveEarningSmallRateLimit :: Amount,
  ctActiveEarningRate :: Rate,
  ctPassiveEarningRate :: Rate,
  ctPassiveDividendRate :: Rate
} deriving (Show, Eq, Generic)

instance ToJSON CorporationRates where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON CorporationRates where
  parseJSON = genericParseJSON jsonOptions

-- Find the salary and corporation social charges to match the earning amount
-- Rounds down the salary to the nearest cent
salaryAndCorporationSocialCharges :: SocialChargesRates -> Amount -> (Salary, SocialCharges)
salaryAndCorporationSocialCharges _ _ = undefined
  -- let x = sort [(scrRRQMaxSalary cs, scrRRQMaxSalary cs *. scrRRQRate cs, True), 
  --               (scrRQAPMaxSalary cs, scrRQAPMaxSalary cs *. scrRQAPRate cs, False)]
  --     (salary1, cs1, max1IsRRQ) = head x
  --     (salary2, cs2, _) = head $ tail x
  --     d1 = salary1 + totalSocialCharges (socialCharges cs salary1)
  --     d2 = salary2 + totalSocialCharges (socialCharges cs salary2)

  --     t1 = (1 + scrRRQRate cs + scrRQAPRate cs + scrFSSRate cs)
  --     t2 = if max1IsRRQ 
  --          then (1 + scrRQAPRate cs + scrFSSRate cs)
  --          else (1 + scrRRQRate cs + scrFSSRate cs)
  --     t3 = (1 + scrFSSRate cs)

  --     s :: Salary
  --     s = if d <= d1
  --         then roundAwayFromZero $ d *. (1 % t1)
  --         else if d <= d2
  --               then (d - cs1) *. (1 / t2)
  --               else (d - cs1 - cs2) *. (1 / t3)
  -- in (s, socialCharges cs s)
