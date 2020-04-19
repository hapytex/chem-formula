{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, OverloadedStrings #-}

module Chemistry.Charge where

import Chemistry.Core(FormulaElement(toFormulaPrec, weight), showParen')

import Data.Text(Text, cons)
import Data.Char.Small(asSup)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), Arbitrary1(liftArbitrary), arbitrary1)

data Charged a
  = Charged a Int
  deriving (Eq, Foldable, Functor, Ord, Read, Show, Traversable)

charged' :: a -> Charged a
charged' = (`Charged` 0)

_charge :: Int -> Text
_charge 0 = ""
_charge (-1) = "\x207a"
_charge n | n < 0 = asSup n
          | otherwise = cons '\x207a' (asSup n)

instance FormulaElement a => FormulaElement (Charged a) where
    toFormulaPrec p (Charged x n) = showParen' (p >= 10) (toFormulaPrec 9 x . (_charge n <>))
    weight (Charged a _) = weight a

instance Arbitrary a => Arbitrary (Charged a) where
    arbitrary = arbitrary1

instance Arbitrary1 Charged where
    liftArbitrary arb = Charged <$> arb <*> arbitrary
