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

import Control.Monad (unless)
import Control.Monad.Except (runExceptT, ExceptT(..), lift, liftEither)
import qualified Data.Yaml as Yaml
import qualified Data.Csv as Csv
import qualified Data.Yaml.Pretty as YamlP
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty as JSONP
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.Char (ord)
import Data.List (sortOn, intercalate)
import Christopher.Taxes
import Christopher.InvestmentTaxation
import Data.Char (toLower)
import System.FilePath (takeExtension)

-- | The commands accepted by the command line interface
data Command = CInvestmentTaxation FilePath

-- | How to execute the CLI commands
runCommand :: Command -> IO ()
runCommand c = runExceptT (runCommand' c) >>= either putStrLn return

runCommand' :: Command -> ExceptT String IO ()
runCommand' (CInvestmentTaxation inputPath) = 
  input <- decodeFileByExt inputPath
  let report = InvestmentTaxation input
  writeToCsv reportPath defaultCsvParam report


-- Decodes with pure JSON for .json file. Any other extension is decoded with in
-- YAML
decodeFileByExt :: (JSON.FromJSON a) => FilePath -> ExceptT String IO a
decodeFileByExt path  = ExceptT $ 
  case map toLower (takeExtension path) of
    ".json" -> JSON.eitherDecodeFileStrict' path
    _ -> fmap (either (Left . show) return) $ Yaml.decodeFileEither path

-- Write
writeReport :: FilePath -> Report -> ExceptT String IO ()
writeReport reportPath journal report = do
  let csvSep = ocsvCsvSeparator $ jCsvOutputParams journal
  let maybeBom = if ocsvAddBom $ jCsvOutputParams journal then bom else ""
  lift $ BL.writeFile reportPath
       $ BL.concat [maybeBom, encodeReportToCsv csvSep report]