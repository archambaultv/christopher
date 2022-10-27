-- |
-- Module      :  Christopher.Internal.BinarySearch
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Christopher.Internal.BinarySearch
(
  binarySearch
)
where

import Data.Ratio ((%))
import Christopher.Amount

-- Finds the amount that satisfy foo x = 0
-- Returns the greater amount if foo s < 0 < foo e with e - s = 1
-- Foo must be increasing in x
binarySearch :: (Amount -> Amount) -> (Amount, Amount) -> Amount
binarySearch _ (s,e) | e < s = error "binarySearch : Upper bound lower than lower bound"
binarySearch _ (s,e) | s == e = s
binarySearch foo (s,e) | e -. s == (Amount 1) =
  let fS = foo s
      fE = foo e
  in if fS == mempty
     then s
     else if fE == mempty
          then e
          else if fS > mempty
               then s
               else e

binarySearch foo (s,e) =
  let m = truncateAmount $ (e +. s) *. (1 % 2)
      fm = foo m
  in case compare fm mempty of
       EQ -> m
       GT -> binarySearch foo (s, m)
       LT -> binarySearch foo (m, e)