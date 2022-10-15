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
import Data.Functor.Foldable (para, ListF(..))
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
break' foo = para alg
  where -- alg :: ListF a ([a], [[a]]) -> [[a]]
        alg Nil = []
        alg (Cons c ([],_)) = [[c]]
        alg (Cons c (_,[])) = [[c]] -- To silence non-exhaustive warning
        alg (Cons c (x:_,(y : ys))) =
          if foo x
          then [c] : y : ys
          else (c : y) : ys