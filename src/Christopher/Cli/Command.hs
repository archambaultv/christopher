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
import Data.Char (toLower)
import System.FilePath (takeExtension)
import Christopher.Report.InvestmentTaxation
import Christopher.Report.TaxTable
import Christopher.Csv

-- | The commands accepted by the command line interface
data Command = CInvestmentTaxation FilePath
             | CTaxTable FilePath (Maybe FilePath)

-- | How to execute the CLI commands
runCommand :: Command -> IO ()
runCommand c = runExceptT (runCommand' c) >>= either putStrLn return

runCommand' :: Command -> ExceptT String IO ()
runCommand' (CInvestmentTaxation inputPath) = do
  input <- decodeFileByExt inputPath
  let report = investmentTaxation input
  lift $ putStrLn $ show report
  -- writeToCsv reportPath defaultCsvParam report

runCommand' (CTaxTable inputPath outputPath) = do
  input <- decodeFileByExt inputPath
  let report = taxTable input
  case outputPath of
    Nothing -> lift $ putStrLn $ show report
    (Just p) -> lift $ writeToCsv p defaultCsvParam report

-- Decodes with pure JSON for .json file. Any other extension is decoded with in
-- YAML
decodeFileByExt :: (JSON.FromJSON a) => FilePath -> ExceptT String IO a
decodeFileByExt path  = ExceptT $ 
  case map toLower (takeExtension path) of
    ".json" -> JSON.eitherDecodeFileStrict' path
    _ -> fmap (either (Left . show) return) $ Yaml.decodeFileEither path