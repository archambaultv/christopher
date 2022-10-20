{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module      :  Christopher.Taxes
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines a combine datatype for personnal and corporation tax

module Christopher.Taxes
(
  Taxes(..),
  tPersonnalTax,
  sortTaxBrackets',
  module Christopher.Taxes.CorporationTax,
  module Christopher.Taxes.Income,
  module Christopher.Taxes.PersonnalTax
)
where

import GHC.Generics
import Data.Aeson (ToJSON(..), FromJSON(..), genericToEncoding, 
                   genericToJSON, genericParseJSON)
import Christopher.Taxes.CorporationTax
import Christopher.Internal.LabelModifier
import Christopher.Taxes.Income
import Christopher.Taxes.PersonnalTax

-- Uses the same name as PersonnalTax so we can load a JSON / YAML file
-- of Taxes as PersonnalTax also
data Taxes = Taxes {
  tCorporationTax :: CorporationTax,
  tFederalPersonnalTax :: FederalPersonnalTax,
  tQuebecPersonnalTax :: QuebecPersonnalTax  
} deriving (Show, Eq, Generic)

tPersonnalTax :: Taxes -> PersonnalTax
tPersonnalTax (Taxes _ f q) = PersonnalTax f q

sortTaxBrackets' :: Taxes -> Taxes
sortTaxBrackets' (Taxes c f q) =
  let (PersonnalTax f' q') = sortTaxBrackets (PersonnalTax f q)
  in Taxes c f' q'

instance ToJSON Taxes where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Taxes where
  parseJSON = genericParseJSON jsonOptions