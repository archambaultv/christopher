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
  InvestmentTaxationRow(..),
  InvestmentTaxationRow'(..),
  investmentTaxation,
  printInvestmentTaxationRow
)
where

import GHC.Generics
import Data.Aeson (ToJSON(..), FromJSON(..), genericToEncoding, 
                   genericToJSON, genericParseJSON)
import Data.Csv
import Data.Functor.Foldable (refold, ListF(..))
import Christopher.Taxes
import Christopher.Internal.LabelModifier
import Christopher.Amount

-- Parameters given by the user
data InvestmentTaxation = InvestmentTaxation {
  itCorporationEarnings :: [Amount],
  itIncomes :: [ITIncome],
  itMarketData :: [MarketData],
  itNumberOfYears :: [Int]
} deriving (Eq, Show, Generic)

-- JSON instances
instance ToJSON InvestmentTaxation where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON InvestmentTaxation where
  parseJSON = genericParseJSON jsonOptions

data ITIncome = ITIncome {
  iiCorporationSalary :: Salary,
  iiOtherSalary :: Salary,
  iiFinalSalary :: Salary
} deriving (Eq, Show, Generic)

-- JSON instances
instance ToJSON ITIncome where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON ITIncome where
  parseJSON = genericParseJSON jsonOptions

data MarketData = MarketData {
  myCapitalGrowth :: Rate,
  myRealisedGainRate :: Rate,
  myDividendYield :: Rate,
  myInterestYield :: Rate,
  myReclaimRDTOH :: Bool
} deriving (Eq, Show, Generic)

-- JSON instances
instance ToJSON MarketData where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON MarketData where
  parseJSON = genericParseJSON jsonOptions


data InvestmentTaxationRow = InvestmentTaxationRow {
  itrEarnings :: Amount,
  itrCorporationPaidInitialSalary :: Salary,
  itrInitalSalary :: Salary, -- This income is not paid by the company
  itrFinalSalary :: Salary, -- This income is not paid by the company
  itrAssetCapitalYield :: Rate,
  itrAssetRealisedCapitalGainRate :: Rate,
  itrDividendYield :: Rate,
  itrInterestYield :: Rate,
  itrNbYears :: Int,
  itrRRSP :: Amount,
  itrTFSA :: Amount,
  itrCorporation :: Amount
} deriving (Eq, Show, Generic)

-- JSON instances
instance ToJSON InvestmentTaxationRow where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON InvestmentTaxationRow where
  parseJSON = genericParseJSON jsonOptions

-- Csv instances
instance FromRecord InvestmentTaxationRow
instance ToRecord InvestmentTaxationRow
instance FromNamedRecord InvestmentTaxationRow where
  parseNamedRecord = genericParseNamedRecord csvOptions
instance ToNamedRecord InvestmentTaxationRow where
  toNamedRecord = genericToNamedRecord csvOptions
instance DefaultOrdered InvestmentTaxationRow where
  headerOrder = genericHeaderOrder csvOptions

data InvestmentTaxationRow' = InvestmentTaxationRow' {
  itrsEarnings :: String,
  itrsCorporationPaidInitialSalary :: String,
  itrsOtherSalary :: String,
  itrsFinalSalary :: String,
  itrsAssetCapitalYield :: String,
  itrsAssetRealisedCapitalGainRate :: String,
  itrsDividendYield :: String,
  itrsInterestYield :: String,
  itrsNbYears :: String,
  itrsRRSP :: String,
  itrsTFSA :: String,
  itrsCorporation :: String
} deriving (Eq, Show, Generic)

-- JSON instances
instance ToJSON InvestmentTaxationRow' where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON InvestmentTaxationRow' where
  parseJSON = genericParseJSON jsonOptions

-- Csv instances
instance FromRecord InvestmentTaxationRow'
instance ToRecord InvestmentTaxationRow'
instance FromNamedRecord InvestmentTaxationRow' where
  parseNamedRecord = genericParseNamedRecord csvOptions
instance ToNamedRecord InvestmentTaxationRow' where
  toNamedRecord = genericToNamedRecord csvOptions
instance DefaultOrdered InvestmentTaxationRow' where
  headerOrder = genericHeaderOrder csvOptions

printInvestmentTaxationRow :: Char -> InvestmentTaxationRow -> InvestmentTaxationRow'
printInvestmentTaxationRow c (InvestmentTaxationRow earn sCorpo sSal fSal acy arcgr
                                                dy iy ny r t cg) = 
  InvestmentTaxationRow'
    (showAmount c earn)
    (showAmount c sCorpo)
    (showAmount c sSal)
    (showAmount c fSal)
    (showRate c acy)
    (showRate c arcgr)
    (showRate c dy)
    (showRate c iy)
    (show ny)
    (showAmount c r)
    (showAmount c t)
    (showAmount c cg)

data InvestmentTaxation' = InvestmentTaxation' {
  it1CorporationEarnings :: Amount,
  it1Incomes :: ITIncome,
  it1MarketData :: MarketData,
  it1NumberOfYears :: Int
} deriving (Eq, Show, Generic)

investmentTaxation :: Taxes -> InvestmentTaxation -> [InvestmentTaxationRow]
investmentTaxation taxes params = 
  let ps = [InvestmentTaxation' e i m y | e <- itCorporationEarnings params,
                                          i <- itIncomes params,
                                          m <- itMarketData params,
                                          y <- itNumberOfYears params]
  in map (invest taxes) ps

invest :: Taxes -> InvestmentTaxation' -> InvestmentTaxationRow
invest t@(Taxes c _ _) (InvestmentTaxation' earning incomes marketData nbYears) =
  let pt = tPersonnalTax t
      r = myCapitalGrowth marketData
      rD = myDividendYield marketData
      rI = myInterestYield marketData
      finalSalary = iiFinalSalary incomes
      otherSalary = iiOtherSalary incomes
      
      -- If we paid the user the earning to invest in TFSA or RRSP, we need to pay
      -- some social charges on this earning. This amount depends on the initial
      -- paid salary

      -- Initial paid salary and social charges
      sCorpo = iiCorporationSalary incomes -- Paid to the user
      initialSocialCharges = socialCharges (ctSocialChargesRates c) sCorpo -- Paid for sCorpo
      initialSalaryEarning = sCorpo + totalSocialCharges initialSocialCharges

      -- Let's find the new salary
      (s2,_) = salaryAndCorporationSocialCharges (ctSocialChargesRates c) (initialSalaryEarning + earning)

      -- Compute RRSP gain
      rrspInitialAmnt = (s2 - sCorpo)
      rrspFinalAmnt = rrspInitialAmnt *. ((1 + r + rD + rI) ^ nbYears)
      rrsp1 = disposableIncome $ computePersonnalTax pt $ PersonnalTaxInput (salary finalSalary) 0
      rrsp2 = disposableIncome $ computePersonnalTax pt $ PersonnalTaxInput (salary (finalSalary + rrspFinalAmnt)) 0
      rrspGain = rrsp2 - rrsp1

      -- Compute TFSA gain
      tfsa1 = disposableIncome $ computePersonnalTax pt $ PersonnalTaxInput (salary (otherSalary + sCorpo)) 0
      tfsa2 = disposableIncome $ computePersonnalTax pt $ PersonnalTaxInput (salary (otherSalary + s2)) 0
      tfsaInitalAmnt = tfsa2 - tfsa1
      tfsaGain = tfsaInitalAmnt *. ((1 + r + rD + rI) ^ nbYears)

      -- Compute Corporation gain
      afterTaxEarnings = disposableEarning $ computeCorporationTax c (activeEarningOnly earning)
      (nonEligibleDiv, taxFreeDiv) = corporationInvestment c marketData nbYears afterTaxEarnings
      r1 = disposableIncome $ computePersonnalTax pt $ PersonnalTaxInput (salary finalSalary) 0
      r2 = disposableIncome 
         $ computePersonnalTax pt 
         $ PersonnalTaxInput (Income finalSalary Nothing (Just nonEligibleDiv)) 0
      corpoGain = taxFreeDiv + r2 - r1     
  in InvestmentTaxationRow earning sCorpo otherSalary finalSalary r (myRealisedGainRate marketData)
                           rD rI nbYears rrspGain tfsaGain corpoGain

corporationInvestment :: CorporationTax -> MarketData -> Int -> Amount -> (Amount, Amount)
corporationInvestment taxes markets y m = 
  sellInvestment $ refold alg coAlg y (initialCorporationInvestmentData m)
  where coAlg :: Int -> ListF Bool Int
        coAlg i = if i <= 0 then Nil else Cons (i == 1) (i - 1)

        -- We get as input : Nb of Share, Price of Share, Adjusted cost price
        alg :: ListF Bool (CorporationInvestmentData -> CorporationInvestmentData) 
            -> CorporationInvestmentData
            -> CorporationInvestmentData
        alg Nil x = x
        alg (Cons z next) x = next
                             $ corporationInvestmentStep taxes markets z x

        sellInvestment :: CorporationInvestmentData -> (Amount, Amount)
        sellInvestment (CorporationInvestmentData _ _ _ rdtoh cda cashOnHand) 
          -- FIXME : Should be limited by the allowed ratio of 0.38 for each dollar
          = (cashOnHand + rdtoh, cda)

data CorporationInvestmentData = CorporationInvestmentData {
  ciNbShare :: Amount,
  ciPrice :: Amount,
  ciAjustedCostBase :: Amount,
  ciRDTOH :: Amount,
  ciCapitalDividendAccount :: Amount,
  ciCashOnHand :: Amount -- Used for the final year
} deriving (Show, Eq)  

corporationInvestmentStep :: CorporationTax 
                          -> MarketData 
                          -> Bool
                          -> CorporationInvestmentData 
                          -> CorporationInvestmentData
corporationInvestmentStep taxes 
                          (MarketData r g rD rI reclaimRdtoh) 
                          lastYear
                          (CorporationInvestmentData nbShare price acb rdtoh cda coh) =
  let -- Interest and dividend gain
      gI = (nbShare * price) *. rI
      gD = (nbShare * price) *. rD

      -- New price and capital gain
      newPrice = price *. (1 + r)
      nbShareSold = if lastYear then nbShare else nbShare *. g
      gC = nbShareSold * (newPrice - acb)

      -- Taxes
      tReport = computeCorporationTax taxes 
              $ CorporationTaxInput 0 gC gD gI rdtoh cda
      (profit, newRdtoh) = if reclaimRdtoh
                           then (disposableEarning tReport + ctRDTOH tReport, 0)
                           else (disposableEarning tReport, ctRDTOH tReport)

      -- Buy new shares and update acb
      nbShareBought = if lastYear then 0 else profit / newPrice
      remainingShare = nbShare - nbShareSold
      newNbShare = remainingShare + nbShareBought
      newAcb = if lastYear then 0 else (remainingShare * acb + profit) / (remainingShare + nbShareBought)
      newCoh = if lastYear then coh + profit else coh
  in CorporationInvestmentData newNbShare newPrice newAcb 
                               newRdtoh 
                               (ctCapitalDividendAccount tReport)
                               newCoh

initialCorporationInvestmentData :: Amount -> CorporationInvestmentData
initialCorporationInvestmentData x = CorporationInvestmentData x 1 1 0 0 0