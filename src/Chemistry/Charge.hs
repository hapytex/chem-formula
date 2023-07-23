{-# LANGUAGE DeriveDataTypeable, DeriveLift, DeriveTraversable, OverloadedStrings #-}

{-|
Module      : Chemistry.Charge
Description : A module that defines datatypes to specify the charge of an atom.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module that defines datatypes to specify the charge of an atoms.
-}
module Chemistry.Charge (
    -- Constructing a charged item.
    Charged(Charged), neutral
  ) where

import Chemistry.Core(FormulaElement(toFormulaPrec, toFormulaPrecColoured, toFormulaMarkupPrec), Weight(weight), showParenText, showParenMarkup)

import Data.Data(Data)
import Data.Text(Text, cons)
import Data.Char.Small(asSup)

import Language.Haskell.TH.Syntax(Lift)

import Text.Blaze(Markup, string)
import Text.Blaze.Html4.Strict(sup)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), Arbitrary1(liftArbitrary), arbitrary1)

-- | A data type that specifies that the given item has a /charge/.
data Charged a
  = Charged a Int -- ^ A dataconstructor that defines a Charged item together with the charge.
  deriving (Data, Eq, Foldable, Functor, Lift, Ord, Read, Show, Traversable)

-- | Construct a 'Charged' item with charge 0.
neutral
  :: a  -- ^ The given item to wrap in a 'Charged' data constructor.
  -> Charged a  -- ^ The corresponding 'Charged' item of the given chemical item.
neutral = (`Charged` 0)

_signify :: Int -> Markup -> Markup
_signify n
  | n < 0 = (sup (string (show n)) <>)
  | n > 0 = (sup (string ('+' : show n)) <>)
  | otherwise = id

_charge :: Int -> Text
_charge 0 = ""
_charge (-1) = "\x207a"
_charge n | n < 0 = asSup n
          | otherwise = cons '\x207a' (asSup n)

instance FormulaElement a => FormulaElement (Charged a) where
    toFormulaPrec p (Charged x n) = showParenText (p >= 6) (toFormulaPrec 5 x . (_charge n <>))
    toFormulaPrecColoured p (Charged x n) = showParenText (p >= 6) (toFormulaPrecColoured 5 x . (_charge n <>))
    toFormulaMarkupPrec p (Charged x n) = showParenMarkup (p >= 6) (toFormulaMarkupPrec 5 x . _signify n)

instance Weight a => Weight (Charged a) where
    weight (Charged a _) = weight a

instance Arbitrary a => Arbitrary (Charged a) where
    arbitrary = arbitrary1

instance Arbitrary1 Charged where
    liftArbitrary arb = Charged <$> arb <*> arbitrary
