-- |
-- Module      :  Christopher.Cli.Commands
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines the commands available when using Christopher

module Christopher.Cli.Command
(
  Command(..),
  runCommand
) where

import Control.Monad.Except (runExceptT, ExceptT(..), lift)
import qualified Data.Yaml as Yaml
import qualified Data.Aeson as JSON
import qualified Data.Csv as Csv
import Data.Char (toLower)
import System.FilePath (takeExtension)
import Christopher.Report.InvestmentTaxation
import Christopher.Report.SalaryOrDividend
import Christopher.Report.TaxTable
import Christopher.Internal.Csv

-- | The commands accepted by the command line interface
data Command = CInvestmentTaxation FilePath FilePath (Maybe FilePath) CsvParam Char
             | CTaxTable FilePath (Maybe FilePath) CsvParam Char
             | CSalaryOrDividend FilePath (Maybe FilePath) CsvParam Char

-- | How to execute the CLI commands
runCommand :: Command -> IO ()
runCommand c = runExceptT (runCommand' c) >>= either putStrLn return

runCommand' :: Command -> ExceptT String IO ()
runCommand' (CInvestmentTaxation taxesPath paramsPath outputPath csvParam decimalSep) = do
  taxes <- decodeFileByExt taxesPath
  params <- decodeFileByExt paramsPath
  let report = map (printInvestmentTaxationRow decimalSep) $ investmentTaxation taxes params
  outputReport outputPath csvParam report

runCommand' (CTaxTable inputPath outputPath csvParam decimalSep) = do
  input <- decodeFileByExt inputPath
  let report = map (printTaxTableRow decimalSep) $ taxTable input
  outputReport outputPath csvParam report

runCommand' (CSalaryOrDividend inputPath outputPath csvParam decimalSep) = do
  input <- decodeFileByExt inputPath
  let report = map (printSalaryOrDividendRow decimalSep) $ salaryOrDividend input
  outputReport outputPath csvParam report

outputReport :: (Csv.ToRecord a) => Maybe FilePath -> CsvParam -> [a] -> ExceptT String IO ()
outputReport outputPath csvParam report =
  case outputPath of
    Nothing -> lift $ putStrLn $ encodeToCsv csvParam report
    (Just p) -> lift $ writeToCsv p csvParam report

-- Decodes with pure JSON for .json file. Any other extension is decoded with in
-- YAML
decodeFileByExt :: (JSON.FromJSON a) => FilePath -> ExceptT String IO a
decodeFileByExt path  = ExceptT $ 
  case map toLower (takeExtension path) of
    ".json" -> JSON.eitherDecodeFileStrict' path
    _ -> fmap (either (Left . show) return) $ Yaml.decodeFileEither path