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
  taxTable
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

taxTable :: IncomeTaxInfo -> [TaxTableRow]
taxTable = undefined