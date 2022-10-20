{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module      :  Christopher.Report.TaxTable
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines some functions to simulate the impact of fiscal policy on investment

module Christopher.Report.SalaryOrDividend(
  SalaryOrDividendRow(..),
  SalaryOrDividendRow'(..),
  salaryOrDividend,
  printSalaryOrDividendRow
)
where

import GHC.Generics
import Data.Aeson (ToJSON(..), FromJSON(..), genericToEncoding, 
                   genericToJSON, genericParseJSON)
import Data.Csv
import Christopher.Taxes
import Christopher.Internal.LabelModifier
import Christopher.Amount

data SalaryOrDividendRow = SalaryOrDividendRow {
  ttrEarnings :: Amount,
  ttrPercentAsSalary :: Rate,
  ttrNetIncome :: Amount
} deriving (Eq, Show, Generic)

-- JSON instances
instance ToJSON SalaryOrDividendRow where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON SalaryOrDividendRow where
  parseJSON = genericParseJSON jsonOptions

-- Csv instances
instance FromRecord SalaryOrDividendRow
instance ToRecord SalaryOrDividendRow
instance FromNamedRecord SalaryOrDividendRow where
  parseNamedRecord = genericParseNamedRecord csvOptions
instance ToNamedRecord SalaryOrDividendRow where
  toNamedRecord = genericToNamedRecord csvOptions
instance DefaultOrdered SalaryOrDividendRow where
  headerOrder = genericHeaderOrder csvOptions

data SalaryOrDividendRow' = SalaryOrDividendRow' {
  ttrsEarnings :: String,
  ttrsSalaryPercent :: String,
  ttrsNetIncome :: String
} deriving (Eq, Show, Generic)

-- JSON instances
instance ToJSON SalaryOrDividendRow' where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON SalaryOrDividendRow' where
  parseJSON = genericParseJSON jsonOptions

-- Csv instances
instance FromRecord SalaryOrDividendRow'
instance ToRecord SalaryOrDividendRow'
instance FromNamedRecord SalaryOrDividendRow' where
  parseNamedRecord = genericParseNamedRecord csvOptions
instance ToNamedRecord SalaryOrDividendRow' where
  toNamedRecord = genericToNamedRecord csvOptions
instance DefaultOrdered SalaryOrDividendRow' where
  headerOrder = genericHeaderOrder csvOptions

printSalaryOrDividendRow :: Char -> SalaryOrDividendRow -> SalaryOrDividendRow'
printSalaryOrDividendRow c (SalaryOrDividendRow earn percent income) = SalaryOrDividendRow'
  (showAmount c earn)
  (showRate c percent)
  (showAmount c income)

salaryOrDividend :: Taxes -> [SalaryOrDividendRow]
salaryOrDividend tax =
  let incomes = [x * 10000 | x <- [1..10]] -- Up to 100K
              ++ [100000 + x * 20000 | x <- [1..15]] -- Up to 400K
      tax' = sortTaxBrackets' tax
  in concatMap (sOrD tax') incomes

sOrD :: Taxes -> Amount -> [SalaryOrDividendRow]
sOrD t@(Taxes corpo _ _) earning = 
  let ps :: [Rate]
      ps = [1,0.8,0.6,0.4,0.2,0] -- % as salary
      -- Maximum salary that can be paid, considering social charges
      foo p =
        let (s, _) = salaryAndCorporationSocialCharges (ctSocialChargesRates corpo) (earning *. p)
            d = disposableEarning corpo (earning *. (1 - p)) 
        in SalaryOrDividendRow earning p (disposableIncome 
                                         $ computePersonnalTax (tPersonnalTax t) 
                                         $ TaxReportInput (Income s (Just 0) (Just d)) 0)
  in map foo ps