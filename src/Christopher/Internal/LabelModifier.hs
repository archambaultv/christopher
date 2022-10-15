-- |
-- Module      :  Christopher.Internal.LabelModifier
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines some helper functions for the JSON format and CSV format

module Christopher.Internal.LabelModifier(
  jsonOptions,
  csvOptions
)
where

import Data.List (intercalate)
import Data.Functor.Foldable (ana, ListF(..))
import Data.Char (toLower, isUpper)
import qualified Data.Aeson as A
import qualified Data.Csv as C

jsonOptions :: A.Options
jsonOptions = A.defaultOptions{
  A.fieldLabelModifier = fieldName,
  A.omitNothingFields = True
}

csvOptions :: C.Options
csvOptions = C.defaultOptions{
  C.fieldLabelModifier = fieldName
}

fieldName :: String -> String
fieldName = map toLower
          . intercalate " "
          . tail -- Drop the data type prefix
          . break' isUpper -- Breaks on each word

break' :: (a -> Bool) -> [a] -> [[a]]
break' foo = ana coAlg
  where -- coAlg :: [a] -> ListF [a] [a]
        coAlg [] = Nil
        coAlg xs = 
          let (c, cs) = break foo xs
          in Cons c cs