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
import Data.Functor.Foldable (cata, para, ListF(..))
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
fieldName = intercalate " "
          . map myToLower
          . restoreAbreviations
          . tail -- Drop the data type prefix
          . break' isUpper -- Breaks on each word

abbv :: [String]
abbv = ["FSS","RRQ","RQAP","RDTOH","RRSP","TFSA"]

myToLower :: String -> String
myToLower x = 
  if x `elem` abbv
  then x
  else map toLower x

-- Restore items of abbv as single word
restoreAbreviations :: [String] -> [String]
restoreAbreviations = cata alg
  where alg :: ListF String [String] -> [String]
        alg Nil = []          
        alg (Cons "F" ("S":"S":xs)) = "FSS" : xs
        alg (Cons "R" ("Q":"A":"P":xs)) = "RQAP" : xs
        alg (Cons "R" ("R":"Q":xs)) = "RRQ" : xs
        alg (Cons "R" ("D":"T":"O":"H":xs)) = "RDTOH" : xs
        alg (Cons "R" ("R":"S":"P":xs)) = "RRSP" : xs
        alg (Cons "T" ("F":"S":"A":xs)) = "TFSA" : xs
        alg (Cons x xs) = x : xs


break' :: (Char -> Bool) -> String -> [String]
break' foo = para alg
  where alg :: ListF Char (String, [String]) -> [String]
        alg Nil = []
        alg (Cons c ([],_)) = [[c]]
        alg (Cons c (_,[])) = [[c]] -- To silence non-exhaustive warning
        alg (Cons c (x:_,(y : ys))) =
          if foo x
          then [c] : y : ys
          else (c : y) : ys