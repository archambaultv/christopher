{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  Christopher.Report.InvestmentTaxation
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines some functions to simulate the impact of fiscal policy on investment

module Christopher.Report.InvestmentTaxation(
  InvestmentTaxation(..),
  itInflationRate,
  itFinalIncome,
  investmentTaxation,
  InvestmentTaxationResult(..),
  printInvestmentTaxation
)
where

import GHC.Generics
import Data.Aeson (ToJSON(..), FromJSON(..), genericToEncoding, 
                   genericToJSON, genericParseJSON)
import Christopher.Taxes
import Christopher.Amount
import Christopher.Internal.LabelModifier

data InvestmentTaxation = InvestmentTaxation {
  itAmount :: Amount,
  itYears :: Int,
  itMarketRate :: Rate,
  itmaybeInflationRate :: Maybe Rate,
  itIncomeTax :: IncomeTaxInfo,
  itInitialIncome :: Income,
  itmaybeFinalIncome :: Maybe Income
} deriving (Show, Eq, Generic)

instance ToJSON InvestmentTaxation where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON InvestmentTaxation where
  parseJSON = genericParseJSON jsonOptions

itInflationRate  :: InvestmentTaxation -> Rate
itInflationRate x = case (itmaybeInflationRate x) of
                      Nothing -> 0
                      Just y -> y

itFinalIncome :: InvestmentTaxation -> Income
itFinalIncome x = case (itmaybeFinalIncome x) of 
                    Nothing -> itInitialIncome x
                    Just y -> y

data InvestmentTaxationResult = InvestmentTaxationResult {
  itData :: InvestmentTaxation,
  itRRSP :: (Amount, Rate),
  itTFSA :: (Amount, Rate)
} deriving (Show, Eq)

printInvestmentTaxation :: Char -> InvestmentTaxationResult -> [[String]]
printInvestmentTaxation c invTax =
  printData (itData invTax)
  ++ [["Investissement","Montant final après taxe","% du montant à placer"]]
  ++ [["REER",showAmount c $ fst $ itRRSP invTax,showRate c $ snd $ itRRSP invTax]]
  ++ [["CELI",showAmount c $ fst $ itTFSA invTax,showRate c $ snd $ itTFSA invTax]]

  where printData _ = []

investmentTaxation :: InvestmentTaxation -> InvestmentTaxationResult
investmentTaxation invTax = InvestmentTaxationResult
  invTax
  (rrspTaxation invTax)
  (tfsaTaxation invTax)

rrspTaxation :: InvestmentTaxation -> (Amount, Rate)
rrspTaxation invTax =
  let r = itMarketRate invTax
      years = itYears invTax
      taxInfo = itIncomeTax invTax

      initialAmount = itAmount invTax

      finalAmount0 = roundTo 2 $ initialAmount *. ((1 + r) ^ years)
      finalTaxInput1 = TaxReportInput (itFinalIncome invTax) 0
      finalAmount1 = afterTaxIncome $ computeTax taxInfo finalTaxInput1
      finalTaxInput2 = TaxReportInput (itFinalIncome invTax <> salary finalAmount0) 0
      finalAmount2 = afterTaxIncome $ computeTax taxInfo finalTaxInput2 
      finalAmount = finalAmount2 - finalAmount1
      ratio = toRate $ roundTo 4 $ finalAmount / initialAmount
  in (finalAmount, ratio)

tfsaTaxation :: InvestmentTaxation -> (Amount, Rate)
tfsaTaxation invTax =
  let r = itMarketRate invTax
      years = itYears invTax
      taxInfo = itIncomeTax invTax
      
      initialTaxInput1 = TaxReportInput (itInitialIncome invTax) 0
      initialAmount1 = afterTaxIncome $ computeTax taxInfo initialTaxInput1
      initialTaxInput2 = TaxReportInput (itInitialIncome invTax <> salary (itAmount invTax)) 0
      initialAmount2 = afterTaxIncome $ computeTax taxInfo initialTaxInput2
      initialAmount = initialAmount2 - initialAmount1

      finalAmount = roundTo 2 $ initialAmount *. ((1 + r) ^ years)
      ratio = toRate $ roundTo 4 $ finalAmount / (itAmount invTax)
  in (finalAmount, ratio)