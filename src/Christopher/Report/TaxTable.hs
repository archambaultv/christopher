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

import GHC.Generics
import Data.Aeson (ToJSON(..), FromJSON(..), genericToEncoding, 
                   genericToJSON, genericParseJSON)
import Data.Csv
import Christopher.Taxes
import Christopher.Internal.LabelModifier
import Christopher.Amount

data TaxTableRow = TaxTableRow {
  ttrTaxableIncome :: Amount,
  ttrFederalTax :: Amount,
  ttrQuebecTax :: Amount,
  ttrTotalTax :: Amount,
  ttrEffectiveRate :: Rate,
  ttrFederalMarginalRate :: Rate,
  ttrQuebecMarginalRate :: Rate,
  ttrTotalMarginalRate :: Rate
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
  ttrsTaxableIncome :: String,
  ttrsFederalTax :: String,
  ttrsQuebecTax :: String,
  ttrsTotalTax :: String,
  ttrsEffectiveRate :: String,
  ttrsFederalMarginalRate :: String,
  ttrsQuebecMarginalRate :: String,
  ttrsTotalMarginalRate :: String
} deriving (Eq, Show, Generic)

printTaxTableRow :: Char -> TaxTableRow -> TaxTableRow'
printTaxTableRow c (TaxTableRow ti ft qt tt ef fmr qmr tmr) = TaxTableRow'
  (showAmount c ti)
  (showAmount c ft)
  (showAmount c qt)
  (showAmount c tt)
  (showRate c ef)
  (showRate c fmr)
  (showRate c qmr)
  (showRate c tmr)

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


marginalSpan :: Amount
marginalSpan = 1000

taxTable :: IncomeTaxInfo -> [TaxTableRow]
taxTable info =
  let incomes = [x * 1000 | x <- [10..80]] -- Up to 80K
              ++ [80000 + x * 5000 | x <- [1..24]] -- Up to 200K
              ++ [200000 + x * 10000 | x <- [1..20]] -- Up to 400K
  in map (\x -> reportToTableRow (computeTax info $ TaxReportInput (salary x) 0,
                                  computeTax info $ TaxReportInput (salary (x + marginalSpan)) 0)) 
     incomes

reportToTableRow :: (TaxReport, TaxReport) -> TaxTableRow
reportToTableRow (tr, marginalTr) =
  let taxableIncome = iSalary $ tiIncome $ trTaxReportInput tr
      fedTax = trFedIncomeTax tr
      qcTax = trQcIncomeTax tr
      totalTax = fedTax + qcTax
      effectiveRate = toRate $ roundTo 4 $ totalTax / taxableIncome

      fedTax2 = trFedIncomeTax marginalTr
      qcTax2 = trQcIncomeTax marginalTr
      totalTax2 = fedTax2 + qcTax2
      fedMarginalRate = toRate $ roundTo 4 $ (fedTax2 - fedTax) / marginalSpan
      qcMarginalRate = toRate $ roundTo 4 $ (qcTax2 - qcTax) / marginalSpan
      totalMarginalRate = toRate $ roundTo 4 $ (totalTax2 - totalTax) / marginalSpan
  in TaxTableRow taxableIncome 
                 (roundTo 0 fedTax) 
                 (roundTo 0 qcTax)
                 (roundTo 0 totalTax) 
                 effectiveRate fedMarginalRate qcMarginalRate totalMarginalRate