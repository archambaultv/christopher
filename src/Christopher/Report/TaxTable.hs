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

module Christopher.Report.TaxTable(
  TaxTableRow(..),
  TaxTableRow'(..),
  taxTable,
  printTaxTableRow
)
where

import Data.List (group, sort)
import GHC.Generics
import Data.Aeson (ToJSON(..), FromJSON(..), genericToEncoding, 
                   genericToJSON, genericParseJSON)
import Data.Csv
import Christopher.Taxes
import Christopher.Internal.LabelModifier
import Christopher.Amount

data TaxTableRow = TaxTableRow {
  ttrTaxableSalary :: Amount,
  ttrFederalTax :: Amount,
  ttrQuebecTax :: Amount,
  ttrTotalTax :: Amount,
  ttrNetIncome :: Amount,
  ttrEffectiveRate :: Rate
} deriving (Eq, Show, Generic)

-- JSON instances
instance ToJSON TaxTableRow where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON TaxTableRow where
  parseJSON = genericParseJSON jsonOptions

-- Csv instances
instance FromRecord TaxTableRow
instance ToRecord TaxTableRow
instance FromNamedRecord TaxTableRow where
  parseNamedRecord = genericParseNamedRecord csvOptions
instance ToNamedRecord TaxTableRow where
  toNamedRecord = genericToNamedRecord csvOptions
instance DefaultOrdered TaxTableRow where
  headerOrder = genericHeaderOrder csvOptions

-- Same as TaxTableRow, but the amount and rate have been printed before hand.
-- To use another decimal separator for example.
data TaxTableRow' = TaxTableRow' {
  ttrsTaxableSalary :: String,
  ttrsFederalTax :: String,
  ttrsQuebecTax :: String,
  ttrsTotalTax :: String,
  ttrsNetIncome :: String,
  ttrsEffectiveRate :: String
} deriving (Eq, Show, Generic)

printTaxTableRow :: Char -> TaxTableRow -> TaxTableRow'
printTaxTableRow c (TaxTableRow ti ft qt tt ni ef) = TaxTableRow'
  (showAmount c ti)
  (showAmount c ft)
  (showAmount c qt)
  (showAmount c tt)
  (showAmount c ni)
  (showRate c ef)

-- JSON instances
instance ToJSON TaxTableRow' where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON TaxTableRow' where
  parseJSON = genericParseJSON jsonOptions

-- Csv instances
instance FromRecord TaxTableRow'
instance ToRecord TaxTableRow'
instance FromNamedRecord TaxTableRow' where
  parseNamedRecord = genericParseNamedRecord csvOptions
instance ToNamedRecord TaxTableRow' where
  toNamedRecord = genericToNamedRecord csvOptions
instance DefaultOrdered TaxTableRow' where
  headerOrder = genericHeaderOrder csvOptions

taxTable :: PersonnalTax -> [TaxTableRow]
taxTable info =
  let incomes = [x * 1000 | x <- [10..80]] -- Up to 80K
              ++ [80000 + x * 5000 | x <- [1..24]] -- Up to 200K
              ++ [200000 + x * 10000 | x <- [1..20]] -- Up to 400K
      incomes2 = map head
               $ group
               $ sort
               $ incomes ++ taxBrackets info
      info' = sortTaxBrackets info
  in map (\x -> reportToTableRow (computePersonnalTax info' $ TaxReportInput (salary x) 0))
     incomes2

reportToTableRow :: TaxReport -> TaxTableRow
reportToTableRow tr =
  let taxableIncome = iSalary $ tiIncome $ trTaxReportInput tr
      fedTax = trFedPersonnalTax tr
      qcTax = trQcPersonnalTax tr
      totalTax = fedTax + qcTax
      effectiveRate = toRate $ roundTo 4 $ totalTax / taxableIncome
  in TaxTableRow taxableIncome 
                 (roundTo 0 fedTax) 
                 (roundTo 0 qcTax)
                 (roundTo 0 totalTax)
                 (taxableIncome - (roundTo 0 totalTax))
                 effectiveRate