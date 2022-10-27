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
import Christopher.Amount
import Christopher.Taxes
import Christopher.Report.SalaryOrDividend
import Christopher.Internal.Csv

-- | The commands accepted by the command line interface
data Command = CForecast FilePath FilePath (Maybe FilePath) CsvParam Char
             | CEffectiveTaxBrackets FilePath (Maybe FilePath) CsvParam Char
             | CSalaryOrDividend FilePath (Maybe FilePath) CsvParam Char
             | CTaxes FilePath Amount (Maybe Amount) (Maybe Amount) (Maybe Amount) Char
             | CSalary FilePath Amount (Maybe Amount) (Maybe Amount) (Maybe Amount) Char
             deriving (Show, Eq)

-- | How to execute the CLI commands
runCommand :: Command -> IO ()
runCommand c = runExceptT (runCommand' c) >>= either putStrLn return

runCommand' :: Command -> ExceptT String IO ()
runCommand' (CForecast taxesPath paramsPath outputPath csvParam decimalSep) = 
  return ()
  -- do
  -- taxes <- decodeFileByExt taxesPath
  -- params <- decodeFileByExt paramsPath
  -- let report = map (printInvestmentTaxationRow decimalSep) $ investmentTaxation taxes params
  -- outputReport outputPath csvParam report

runCommand' (CEffectiveTaxBrackets inputPath outputPath csvParam decimalSep) = 
  return ()
  --do
--   input <- decodeFileByExt inputPath
--   let report = map (printTaxTableRow decimalSep) $ taxTable input
--   outputReport outputPath csvParam report

runCommand' (CSalaryOrDividend inputPath outputPath csvParam decimalSep) = 
  return ()
  --do
  -- input <- decodeFileByExt inputPath
  -- let report = map (printSalaryOrDividendRow decimalSep) $ salaryOrDividend input
  -- outputReport outputPath csvParam report

runCommand' (CTaxes inputPath s d o rrsp decimalSep) = do
  input <- decodeFileByExt inputPath
  let r = computePersonnalTax input 
        $ PersonnalTaxInput (Income s d o) (maybe mempty id rrsp)
  let pt = showAmount decimalSep (personnalTax r)
  let di = showAmount decimalSep (disposableIncome r)
  let n = max (length pt) (length di)
  lift $ putStrLn $ "           Taxes : " ++ lpad n pt
  lift $ putStrLn $ "After tax income : " ++ lpad n di

runCommand' (CSalary inputPath r d o rrsp decimalSep) = do
  input <- decodeFileByExt inputPath
  let s = findSalary input r (maybe mempty id d) (maybe mempty id o) (maybe mempty id rrsp)
  let t = computePersonnalTax input 
        $ PersonnalTaxInput (Income s d o) (maybe mempty id rrsp)
  let ts = showAmount decimalSep (personnalTax t)
  let ss = showAmount decimalSep s
  let n = max (length ts) (length ss)
  lift $ putStrLn $ " Taxes : " ++ lpad n ts
  lift $ putStrLn $ "Salary : " ++ lpad n ss

lpad :: Int -> String -> String
lpad n xs = take (n - length xs) (repeat ' ') ++ xs

outputReport :: (Csv.ToNamedRecord a, Csv.DefaultOrdered a) 
             => Maybe FilePath -> CsvParam -> [a] -> ExceptT String IO ()
outputReport outputPath csvParam report =
  case outputPath of
    Nothing -> lift $ putStrLn $ encodeToNamedCsv csvParam report
    (Just p) -> lift $ writeToNamedCsv p csvParam report

-- Decodes with pure JSON for .json file. Any other extension is decoded with in
-- YAML
decodeFileByExt :: (JSON.FromJSON a) => FilePath -> ExceptT String IO a
decodeFileByExt path  = ExceptT $ 
  case map toLower (takeExtension path) of
    ".json" -> JSON.eitherDecodeFileStrict' path
    _ -> fmap (either (Left . show) return) $ Yaml.decodeFileEither path