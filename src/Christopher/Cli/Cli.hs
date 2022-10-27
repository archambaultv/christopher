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
import Christopher.Amount
import Christopher.Cli.Command
import Christopher.Internal.Csv (CsvParam(..))
import Data.Scientific (scientificP)
import Text.ParserCombinators.ReadP (readP_to_S)

charReader :: ReadM Char
charReader = eitherReader
           $ \s -> case s of {[c] -> return c; _ -> Left "Expecting a single character"}

amountReader :: ReadM Amount
amountReader = eitherReader
             $ \s -> case last (readP_to_S scientificP s) of
                        (x,"") -> case fromScientific x of 
                                      Just c -> return c
                                      _ -> Left "Expecting an amount (max 2 decimal places)"
                        _ -> Left "Expecting an amount (max 2 decimal places)"

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
investmentTaxation = CForecast
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
taxTable = CEffectiveTaxBrackets
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

taxes :: Parser Command
taxes = CTaxes
      <$> taxesFile
      <*> salary 
      <*> optional eligible 
      <*> optional noneligible 
      <*> optional rrsp
      <*> decimalSeparator

salary :: Parser Amount
salary = argument amountReader (metavar "SALARY" <> help "The amount of salary")
eligible :: Parser Amount
eligible = argument amountReader (metavar "ELIGIBLE DIVIDEND" <> help "The amount of eligible dividend")
noneligible :: Parser Amount
noneligible = argument amountReader (metavar "NON ELIGIBLE DIVIDEND" <> help "The amount of non eligible dividend")
rrsp :: Parser Amount
rrsp = argument amountReader (metavar "RRSP" <> help "The contribution to RRSP")   
revenue :: Parser Amount
revenue = argument amountReader (metavar "AFTER-TAX-INCOME" <> help "The amount of after tax income")

taxesInfo :: ParserInfo Command
taxesInfo = info (taxes <**> helper)
              (fullDesc
               <> progDesc "Computes personnal taxes")

salaryP :: Parser Command
salaryP = CSalary
      <$> taxesFile
      <*> revenue 
      <*> optional eligible 
      <*> optional noneligible 
      <*> optional rrsp
      <*> decimalSeparator


salaryInfo :: ParserInfo Command
salaryInfo = info (salaryP <**> helper)
              (fullDesc
               <> progDesc "Computes the salary needed to reach the requested revenue")

parseCommand :: Parser Command
parseCommand = subparser
  ( command "forecast" investmentTaxationInfo <>
    command "effective-tax-brackets" taxTableInfo <>
    command "salary-or-dividend" salaryOrDividendInfo <>
    command "taxes" taxesInfo <>
    command "salary" salaryInfo
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