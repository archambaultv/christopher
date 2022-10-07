-- |
-- Module      :  Main
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- The main module of the executable

module Main (main) where

main :: IO ()
main = putStrLn msg

msg :: String
msg = "Hello my name is Christopher. I am a simple tool to help you plan your financial future.\n\n"
   ++ "Looks like your are running the default command line executable. "
   ++ "For now, in order to use this tool you need to code your own simulation (see the Simulation data type) and modify the main function.\n\n"
   ++ "I suggest you do so in your own private branch, since your simulations might contain personnal financial data."
