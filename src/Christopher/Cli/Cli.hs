-- |
-- Module      :  Christopher.Cli.Cli
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines the command line interface of Christopher

module Christopher.Cli.Cli
(
  cli
)

where

import Options.Applicative
import Christopher.Cli.Command
import Christopher.Internal.Csv (CsvParam(..))

charReader :: ReadM Char
charReader = eitherReader
           $ \s -> case s of {[c] -> return c; _ -> Left "Expecting a single character"}

taxesFile :: Parser String
taxesFile = argument str (metavar "INPUT-FILE" <> help "The JSON or YAML file containing tax related parameters.")

outputFile :: Parser String
outputFile = argument str (metavar "CSV-FILE" <> help "The output file in CSV format.")

decimalSeparator :: Parser Char
decimalSeparator = option charReader (short 'd' <>
                          long "decimal-sep" <>
                          metavar "DECIMAL-SEPARATOR" <>
                          showDefault <>
                          value ',' <>
                          help "Specify the decimal separator to use.") 

csvParam :: Parser CsvParam
csvParam = (CsvParam . not)
         <$> switch (long "nobom" <>
                     help "Do not add the Byte Order Mark to the csv file.")
         <*> option charReader (short 's' <>
                          long "csv-sep" <>
                          metavar "CSV-SEPARATOR" <>
                          showDefault <>
                          value ';' <>
                          help "Specify the CSV separator to use.") 

investmentTaxation :: Parser Command
investmentTaxation = CInvestmentTaxation
                   <$> taxesFile
                   <*> argument str (metavar "TAXES-FILE" <> help "The JSON or YAML file containing the simulation parameters.")
                   <*> optional outputFile
                   <*> csvParam
                   <*> decimalSeparator

investmentTaxationInfo :: ParserInfo Command
investmentTaxationInfo = info (investmentTaxation <**> helper)
              (fullDesc
               <> progDesc "Computes the effective taxation of various investments according to the parameters in the INPUT-FILE.")

taxTable :: Parser Command
taxTable = CTaxTable
        <$> taxesFile
        <*> optional outputFile
        <*> csvParam
        <*> decimalSeparator

taxTableInfo :: ParserInfo Command
taxTableInfo = info (taxTable <**> helper)
              (fullDesc
               <> progDesc "Computes the tax table according to the fiscal parameters in the INPUT-FILE.")

salaryOrDividend :: Parser Command
salaryOrDividend = CSalaryOrDividend
                <$> taxesFile
                <*> optional outputFile
                <*> csvParam
                <*> decimalSeparator

salaryOrDividendInfo :: ParserInfo Command
salaryOrDividendInfo = info (salaryOrDividend <**> helper)
              (fullDesc
               <> progDesc "Computes if it is better to pay a salary or dividend to the fiscal parameters in the INPUT-FILE.")

parseCommand :: Parser Command
parseCommand = subparser
  ( command "investment-taxation" investmentTaxationInfo <>
    command "tax-table" taxTableInfo <>
    command "salary-or-dividend" salaryOrDividendInfo
  )

opts :: ParserInfo Command
opts = info (parseCommand <**> helper)
       (fullDesc
         <> progDesc "Executes COMMAND. Use christopher -h to list the \
                     \possible commands. Use christopher COMMAND -h for help \
                     \on a specific command."
         <> header "christopher - Financial planning command line tool")

cli :: IO ()
cli = execParser opts >>= runCommand