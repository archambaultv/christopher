{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module      :  Christopher.Taxes.Income
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines what is an income

module Christopher.Taxes.Income
(
  Salary,
  Dividend,
  Income(..),
  iEligibleDividend,
  iNonEligibleDividend,
  salary,
) where

import GHC.Generics
import Data.Aeson (ToJSON(..), FromJSON(..), genericToEncoding, 
                   genericToJSON, genericParseJSON)
import Christopher.Amount
import Christopher.Internal.LabelModifier

type Salary = Amount
type Dividend = Amount

data Income = Income {
  iSalary :: Salary,
  imEligibleDividend :: Maybe Dividend, -- Determiné
  imNonEligibleDividend :: Maybe Dividend
} deriving (Show, Eq, Generic)

iEligibleDividend :: Income -> Dividend
iEligibleDividend (Income _ Nothing _) = mempty
iEligibleDividend (Income _ (Just d) _) = d

iNonEligibleDividend :: Income -> Dividend
iNonEligibleDividend (Income _ _ Nothing) = mempty
iNonEligibleDividend (Income _ _ (Just d)) = d

salary :: Amount -> Income
salary x = Income x Nothing Nothing

instance ToJSON Income where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Income where
  parseJSON = genericParseJSON jsonOptions

instance Semigroup Income where
  i1 <> i2 = Income (iSalary i1 +. iSalary i2)
                    (addMaybe (imEligibleDividend i1) (imEligibleDividend i2))
                    (addMaybe (imNonEligibleDividend i1) (imNonEligibleDividend i2))

addMaybe :: Maybe Amount -> Maybe Amount -> Maybe Amount
addMaybe Nothing Nothing = Nothing
addMaybe (Just x) Nothing = Just x
addMaybe Nothing (Just x) = Just x
addMaybe (Just x) (Just y) = Just (x +. y)

instance Monoid Income where
  mempty = Income mempty Nothing Nothing