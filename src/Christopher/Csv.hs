{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Christopher.Csv
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines some helpers to export data to the csv format

module Christopher.Csv
(
  CsvParam(..),
  defaultCsvParam,
  excelFrenchCanadaCsvParam,
  writeToCsv
)
where

import Data.Char (ord)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv

bom :: BL.ByteString
bom = BL.pack [0xEF,0xBB,0xBF]

data CsvParam = CsvParam {
  addBom :: Bool,
  separator :: Char
}

defaultCsvParam :: CsvParam
defaultCsvParam = CsvParam False ','

excelFrenchCanadaCsvParam :: CsvParam
excelFrenchCanadaCsvParam = CsvParam True ';'

writeToCsv :: (Csv.ToRecord a) => FilePath -> Maybe CsvParam -> [a] -> IO ()
writeToCsv path Nothing x = writeToCsv path (Just defaultCsvParam) x
writeToCsv path (Just (CsvParam b c)) x =
  let maybeBom = if b then bom else ""
      myOptions = Csv.defaultEncodeOptions {
                      Csv.encDelimiter = fromIntegral (ord c)
                    }
      x' = Csv.encodeWith myOptions x
  in BL.writeFile path $ BL.concat [maybeBom, x']