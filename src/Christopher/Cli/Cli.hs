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

InvestmentTaxation :: Parser Command
InvestmentTaxation = CInvestmentTaxation
                   <$> inputFile

InvestmentTaxation :: ParserInfo Command
InvestmentTaxation = info (InvestmentTaxation <**> helper)
              (fullDesc
               <> progDesc "Computes the effective fiscal rate according to the parameters in the INPUT-FILE.")

parseCommand :: Parser Command
parseCommand = subparser
  ( command "investment-taxation" InvestmentTaxation
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