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

inputFile :: Parser String
inputFile = argument str (metavar "INPUT-FILE" <> help "The input file in JSON or YAML format.")

outputFile :: Parser String
outputFile = argument str (metavar "CSV-FILE" <> help "The output file in CSV format.")

investmentTaxation :: Parser Command
investmentTaxation = CInvestmentTaxation
                   <$> inputFile

investmentTaxationInfo :: ParserInfo Command
investmentTaxationInfo = info (investmentTaxation <**> helper)
              (fullDesc
               <> progDesc "Computes the effective taxation of various investments according to the parameters in the INPUT-FILE.")

taxTable :: Parser Command
taxTable = CTaxTable
        <$> inputFile
        <*> optional outputFile

taxTableInfo :: ParserInfo Command
taxTableInfo = info (taxTable <**> helper)
              (fullDesc
               <> progDesc "Computes the tax table according to the parameters in the INPUT-FILE.")

parseCommand :: Parser Command
parseCommand = subparser
  ( command "investment-taxation" investmentTaxationInfo <>
    command "tax-table" taxTableInfo
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