-- |
-- Module      :  Christopher.Utils
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines some helper functions

module Christopher.Utils(
  jsonOptions,

)
where

import Data.List (intercalate)
import Data.Functor.Foldable (ana, ListF(..))
import Data.Char (toLower, isUpper)
import Data.Aeson (defaultOptions, Options(..))

jsonOptions :: Options
jsonOptions = defaultOptions{
  fieldLabelModifier = fieldName,
  omitNothingFields = True
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