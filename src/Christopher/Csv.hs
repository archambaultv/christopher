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
  writeToCsv,
  toISO8601
)
where

import Data.Time (Day, formatTime, defaultTimeLocale, iso8601DateFormat)
import Data.Char (ord)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv

bom :: BL.ByteString
bom = BL.pack [0xEF,0xBB,0xBF]

data CsvParam = CsvParam {
  addBom :: Bool,
  separator :: Char
} deriving (Show, Eq)

defaultCsvParam :: CsvParam
defaultCsvParam = CsvParam False ','

writeToCsv :: (Csv.ToRecord a) => FilePath -> CsvParam -> [a] -> IO ()
writeToCsv path (CsvParam b c) x =
  let maybeBom = if b then bom else ""
      myOptions = Csv.defaultEncodeOptions {
                      Csv.encDelimiter = fromIntegral (ord c)
                    }
      x' = Csv.encodeWith myOptions x
  in BL.writeFile path $ BL.concat [maybeBom, x']

-- | Helper to format a date in ISO 8601 format
toISO8601 :: Day -> String
toISO8601 = formatTime defaultTimeLocale (iso8601DateFormat Nothing)