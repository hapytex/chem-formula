{-# LANGUAGE DeriveDataTypeable, DeriveLift, DeriveTraversable, OverloadedStrings #-}

module Chemistry.Charge where

import Chemistry.Core(FormulaElement(toFormulaPrec), Weight(weight), showParen')

import Data.Data(Data)
import Data.Text(Text, cons)
import Data.Char.Small(asSup)

import Language.Haskell.TH.Syntax(Lift)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), Arbitrary1(liftArbitrary), arbitrary1)

data Charged a
  = Charged a Int
  deriving (Data, Eq, Foldable, Functor, Lift, Ord, Read, Show, Traversable)

charged' :: a -> Charged a
charged' = (`Charged` 0)

_charge :: Int -> Text
_charge 0 = ""
_charge (-1) = "\x207a"
_charge n | n < 0 = asSup n
          | otherwise = cons '\x207a' (asSup n)

instance FormulaElement a => FormulaElement (Charged a) where
    toFormulaPrec p (Charged x n) = showParen' (p >= 6) (toFormulaPrec 5 x . (_charge n <>))

instance Weight a => Weight (Charged a) where
    weight (Charged a _) = weight a

instance Arbitrary a => Arbitrary (Charged a) where
    arbitrary = arbitrary1

instance Arbitrary1 Charged where
    liftArbitrary arb = Charged <$> arb <*> arbitrary
