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
  computeEarningTax,
  disposableEarning,
  salaryAndCorporationSocialCharges
)
where

import GHC.Generics
import Data.List (sort)
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

computeEarningTax :: CorporationTax -> Amount -> Amount
computeEarningTax (CorporationTax fed qc _ _ _) x = 
  let foo corpo =
        let small = min x (crActiveEarningSmallRateLimit corpo)
            above = max 0 (x - (crActiveEarningSmallRateLimit corpo))
        in small *. (crActiveEarningSmallRate corpo)
           + above *. (ctActiveEarningRate corpo)
  in foo fed + foo qc

disposableEarning :: CorporationTax -> Amount -> Amount
disposableEarning tax x = x - computeEarningTax tax x

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
salaryAndCorporationSocialCharges cs d =
  let x = sort [(scrRRQMaxSalary cs, scrRRQMaxSalary cs *. scrRRQRate cs, True), 
                (scrRQAPMaxSalary cs, scrRQAPMaxSalary cs *. scrRQAPRate cs, False)]
      (salary1, cs1, max1IsRRQ) = head x
      (salary2, cs2, _) = head $ tail x
      d1 = salary1 + totalSocialCharges (socialCharges cs salary1)
      d2 = salary2 + totalSocialCharges (socialCharges cs salary2)

      t1 = (1 + scrRRQRate cs + scrRQAPRate cs + scrFSSRate cs)
      t2 = if max1IsRRQ 
           then (1 + scrRQAPRate cs + scrFSSRate cs)
           else (1 + scrRRQRate cs + scrFSSRate cs)
      t3 = (1 + scrFSSRate cs)

      s :: Salary
      s = if d <= d1
          then d *. (1 / t1)
          else if d <= d2
                then (d - cs1) *. (1 / t2)
                else (d - cs1 - cs2) *. (1 / t3)
  in (s, socialCharges cs s)
