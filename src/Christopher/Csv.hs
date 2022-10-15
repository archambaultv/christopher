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
  standardCsvParam,
  xlCanadaFrenchCsvParam,
  encodeToCsv,
  writeToCsv,
  toISO8601
)
where

import Data.Text.Encoding (decodeUtf8)
import Data.Time (Day, formatTime, defaultTimeLocale, iso8601DateFormat)
import Data.Char (ord)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Text as T

bom :: BL.ByteString
bom = BL.pack [0xEF,0xBB,0xBF]

data CsvParam = CsvParam {
  addBom :: Bool,
  separator :: Char
} deriving (Show, Eq)

standardCsvParam :: CsvParam
standardCsvParam = CsvParam False ','

xlCanadaFrenchCsvParam :: CsvParam
xlCanadaFrenchCsvParam = CsvParam True ';'

writeToCsv :: (Csv.ToNamedRecord a, Csv.DefaultOrdered a) => FilePath -> CsvParam -> [a] -> IO ()
writeToCsv path (CsvParam b c) x =
  let maybeBom = if b then bom else ""
      myOptions = Csv.defaultEncodeOptions {
                      Csv.encDelimiter = fromIntegral (ord c)
                    }
      x' = Csv.encodeDefaultOrderedByNameWith myOptions x
  in BL.writeFile path $ BL.concat [maybeBom, x']

encodeToCsv :: (Csv.ToNamedRecord a, Csv.DefaultOrdered a) => CsvParam -> [a] -> String
encodeToCsv (CsvParam _ c) x =
  let myOptions = Csv.defaultEncodeOptions {
                      Csv.encDelimiter = fromIntegral (ord c)
                    }
  in T.unpack $ decodeUtf8 $ BL.toStrict $ Csv.encodeDefaultOrderedByNameWith myOptions x

-- | Helper to format a date in ISO 8601 format
toISO8601 :: Day -> String
toISO8601 = formatTime defaultTimeLocale (iso8601DateFormat Nothing)