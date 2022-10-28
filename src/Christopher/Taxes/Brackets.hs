{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module      :  Christopher.Taxes.PersonnalTax
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--

module Christopher.Taxes.Brackets (
  TaxBrackets(..),
  TaxBracket(..),
  sortTaxBrackets,
  applyBrackets,
  vShiftBrackets,
  mergeBrackets,
  affinityBrackets,
  floorBrackets
)
where

import GHC.Generics
import Data.List (sortOn)
import Data.Functor.Foldable
import Data.Aeson (ToJSON(..), FromJSON(..), genericToEncoding, 
                   genericToJSON, genericParseJSON)
import Christopher.Amount
import Christopher.Internal.LabelModifier

-- Continous monotically non decreasing piecewise function 
-- Brackets limit must be in increasing order of tbAbove
-- Rate must be positive
data TaxBrackets = TaxBrackets {
  tbBaseRate :: Rate,
  tbBaseAmount :: Amount,
  tbBrackets :: [TaxBracket]
} deriving (Eq, Show, Generic)

data TaxBracket = TaxBracket {
  tbAbove :: Amount,
  tbRate :: Rate
}
 deriving (Show, Eq, Generic)

instance ToJSON TaxBrackets where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON TaxBrackets where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TaxBracket where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON TaxBracket where
  parseJSON = genericParseJSON jsonOptions

-- (vShiftBrackets B k)(s) = B(s) + k
vShiftBrackets :: TaxBrackets -> Amount -> TaxBrackets
vShiftBrackets (TaxBrackets r a0 xs) a = (TaxBrackets r (a0 +. a) xs)

-- (floorBrackets B k)(s) = max(k, B(s))
floorBrackets :: TaxBrackets -> Amount -> TaxBrackets
floorBrackets (TaxBrackets r a0 xs) k | k <= a0 = (TaxBrackets r a0 xs)
floorBrackets (TaxBrackets r a0 xs) k =
  let xs' = para alg (TaxBracket mempty r : xs) (amountToRational $ k -. a0)
  in TaxBrackets 0 k xs'

  where alg :: ListF TaxBracket ([TaxBracket], Rational -> [TaxBracket]) -> (Rational -> [TaxBracket])
        alg Nil _ = []
        alg (Cons (TaxBracket limit rate) ([], _)) acc = 
          let l = truncateAmount $ acc / rate 
          in (TaxBracket (limit +. l) rate) : xs
        alg (Cons (TaxBracket limit rate) (ys@(TaxBracket nextLimit _ : _), next)) acc =
          let l = truncateAmount $ acc / rate
          in if l <= nextLimit -. limit
             then (TaxBracket (limit +. l) rate) : ys
             else next (acc - (nextLimit -. limit) *. rate)

-- (mergeBrackets B1 B2)(s) = B1(s) + B2(s)
mergeBrackets :: TaxBrackets -> TaxBrackets -> TaxBrackets
mergeBrackets (TaxBrackets r01 a01 xs01) (TaxBrackets r02 a02 xs02) =
  let xs = ana coalg ((r01, xs01), (r02, xs02))
  in (TaxBrackets (r01 + r02) (a01 +. a02) xs)

  where coalg :: ((Rate, [TaxBracket]),(Rate, [TaxBracket]))
              -> (ListF TaxBracket ((Rate, [TaxBracket]),(Rate, [TaxBracket])))
        coalg ((_,[]),(_,[])) = Nil

        coalg ((r1,[]),(r2,(TaxBracket m r):xs)) = 
            Cons (TaxBracket m (r + r1)) ((r1,[]),(r2,xs)) 
        
        coalg ((r1,(TaxBracket m r):xs),(r2,[])) = 
            Cons (TaxBracket m (r + r2)) ((r1,xs),(r2,[])) 
        
        coalg ((r1,x1:xs1),(r2,x2:xs2)) =
          let newR1 = tbRate x1
              newR2 = tbRate x2
              l1 = tbAbove x1
              l2 = tbAbove x2
          in case compare l1 l2 of
              EQ -> Cons (TaxBracket l1 (newR1 + newR2)) ((newR1,xs1),(newR2,xs2)) 
              LT -> Cons (TaxBracket l1 (newR1 + r2)) ((newR1,xs1),(r2,x2:xs2)) 
              GT -> Cons (TaxBracket l2 (r1 + newR2)) ((r1,x1:xs1),(newR2,xs2)) 

-- (affinityBrackets B1 m b)(s) = B1(m*s + b)
affinityBrackets :: TaxBrackets -> Rate -> Amount -> TaxBrackets
affinityBrackets tb@(TaxBrackets r0 _ xs) m b =
  let xs' = map foo xs
      b0' = applyBrackets tb b
  in (TaxBrackets (r0 * m) b0' xs')

  where foo :: TaxBracket -> TaxBracket
        foo (TaxBracket l r) = 
          let l' = truncateAmount (l *. (1 / m)) +. b
          in TaxBracket l' (r * m)
       
applyBrackets :: TaxBrackets -> Amount -> Amount 
applyBrackets (TaxBrackets baseRate baseAmount brackets) x = 
  roundAwayFromZero
  -- FIXME for lower than first bracket
  $ para alg brackets (x *. baseRate + toRational baseAmount)
  where
    alg :: ListF TaxBracket ([TaxBracket], Rational -> Rational) -> (Rational -> Rational) 
    alg Nil acc = acc
    alg (Cons (TaxBracket limit rate) ([], _)) acc = 
      acc + (x -. limit) *. rate
    alg (Cons (TaxBracket limit rate) ((TaxBracket nextLimit _ : _), next)) acc =
      if x <= nextLimit
      then -- This bracket if the final stop
           acc + (x -. limit) *. rate
      else  
           -- Compute this bracket and move on to the next
        let acc' = acc + (nextLimit -. limit) *. rate
        in next acc'

sortTaxBrackets :: TaxBrackets -> TaxBrackets
sortTaxBrackets (TaxBrackets r a xs) = TaxBrackets r a (sortOn tbAbove xs)