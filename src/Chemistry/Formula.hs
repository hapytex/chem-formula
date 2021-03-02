{-# LANGUAGE DeriveDataTypeable, DeriveLift, DeriveTraversable, FlexibleInstances, OverloadedStrings, TypeFamilies #-}

{-|
Module      : Chemistry.Formula
Description : A module that defines datastructures to present formulas and linear chains.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module that defines datastructure to present formulas and linear chains.
-}

module Chemistry.Formula where

import Chemistry.Bond(Bond(BSingle, BDouble, BTriple, BQuadruple), bondToUnicode)
import Chemistry.Core(
    FormulaElement(toFormulaPrec, toFormulaMarkupPrec), HillCompare(hillCompare), QuantifiedElements(listElementsCounter, foldQuantified)
  , Weight(weight), quantifiedWeight, showParenText, showParenMarkup
  )

import Data.Char.Small(asSub)
import Data.Data(Data)
import Data.Default(Default(def))
import Data.Hashable(Hashable)
import qualified Data.HashMap.Strict as HM
import Data.Function(on)
import Data.List(sortBy)
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Text(cons, singleton)

import GHC.Exts(IsList(Item, fromList, toList))

import Language.Haskell.TH.Syntax(Lift)

import Text.Blaze(string, text)
import Text.Blaze.Html4.Strict(sub)

import Test.QuickCheck(Gen, oneof)
import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), Arbitrary1(liftArbitrary), Arbitrary2(liftArbitrary2), arbitrary1, arbitrary2)

infix 8 :*
infixr 8 .*
infixr 7 :-
infixr 6 .-
infixr 6 .=
infixr 6 .#
infixr 6 .$

data FormulaPart a
  = FormulaItem a
  | (Formula a) :* Int
  deriving (Data, Eq, Foldable, Functor, Lift, Ord, Read, Show, Traversable)

data Formula a
  = FormulaPart (FormulaPart a)
  | (FormulaPart a) :- (Formula a)
  deriving (Data, Eq, Foldable, Functor, Lift, Ord, Read, Show, Traversable)

data LinearChain bond element
  = ChainItem element
  | Chain element bond (LinearChain bond element)
  deriving (Data, Eq, Foldable, Functor, Lift, Ord, Read, Show, Traversable)

(.*) :: FormulaPart a -> Int -> FormulaPart a
(.*) = (:*) . FormulaPart

(.-) :: element -> LinearChain Bond element -> LinearChain Bond element
(.-) = (`Chain` BSingle)

(.=) :: element -> LinearChain Bond element -> LinearChain Bond element
(.=) = (`Chain` BDouble)

(.#) :: element -> LinearChain Bond element -> LinearChain Bond element
(.#) = (`Chain` BTriple)

(.$) :: element -> LinearChain Bond element -> LinearChain Bond element
(.$) = (`Chain` BQuadruple)

formulaToParts :: Formula a -> NonEmpty (FormulaPart a)
formulaToParts (FormulaPart p) = p :| []
formulaToParts (p :- f) = p :| toList f

instance IsList (Formula a) where
    type Item (Formula a) = (FormulaPart a)
    fromList [] = error "A formula contains at least one element"
    fromList (fp:fps) = go fps fp
        where go [] = FormulaPart
              go (x:xs) = (:- go xs x)
    toList (FormulaPart p) = [p]
    toList (p :- f) = p : toList f

instance Default bond => IsList (LinearChain bond element) where
    type Item (LinearChain bond element) = element
    fromList [] = error "A linear chain contains at least one element"
    fromList (e:es) = go es e
        where go [] = ChainItem
              go (c:cs) = (`h` go cs c)
              h = (`Chain` def)
    toList (ChainItem e) = [e]
    toList (Chain e _ es) = e : toList es

instance Semigroup (Formula a) where
    (<>) (FormulaPart p) = (p :-)
    (<>) (f :- p) = (f :-) . (p <>)


fromElementList :: [(a, Int)] -> Formula a
fromElementList = fromList . map (uncurry ((:*) . FormulaPart . FormulaItem))

_processFormula :: (Eq a, Hashable a) => ([(a, Int)] -> [(a, Int)]) -> Formula a -> Formula a
_processFormula f = fromElementList . f . HM.toList . listElementsCounter

toMolecular :: (Eq a, Hashable a) => Formula a -> Formula a
toMolecular = _processFormula id

toHillFormula :: (Eq a, Hashable a, HillCompare a) => Formula a -> Formula a
toHillFormula = _processFormula (sortBy (hillCompare `on` fst))

instance FormulaElement a => FormulaElement (FormulaPart a) where
    toFormulaPrec p (FormulaItem e) = toFormulaPrec p e
    toFormulaPrec p (f :* 1) = showParenText (p >= 5) (toFormulaPrec 5 f)
    toFormulaPrec p (f :* n) = showParenText (p >= 5) (toFormulaPrec 5 f . (asSub n <>))
    toFormulaMarkupPrec p (FormulaItem e) = toFormulaMarkupPrec p e
    toFormulaMarkupPrec p (f :* 1) = showParenMarkup (p >= 5) (toFormulaMarkupPrec p f)
    toFormulaMarkupPrec p (f :* n) = showParenMarkup (p >= 5) (toFormulaMarkupPrec p f . (sub (string (show n)) <>))

instance QuantifiedElements FormulaPart where
    foldQuantified f g h = go
        where go (FormulaItem a) = f a
              go (fp :* n) = h n (go' fp)
              go' (FormulaPart a) = go a
              go' (fp :- frm) = g (go fp) (go' frm)

instance Weight a => Weight (FormulaPart a) where
    weight = quantifiedWeight

instance QuantifiedElements Formula where
    foldQuantified f g h = go'
        where go (FormulaItem a) = f a
              go (fp :* n) = h n (go' fp)
              go' (FormulaPart a) = go a
              go' (fp :- frm) = g (go fp) (go' frm)

instance FormulaElement a => FormulaElement (Formula a) where
    toFormulaPrec p' (FormulaPart p) = toFormulaPrec p' p
    toFormulaPrec p' (p :- f) = showParenText (p' >= 4) (toFormulaPrec 3 p . toFormulaPrec 3 f)
    toFormulaMarkupPrec p' (FormulaPart p) = toFormulaMarkupPrec p' p
    toFormulaMarkupPrec p' (p :- f) = showParenMarkup (p' >= 4) (toFormulaMarkupPrec 3 p . toFormulaMarkupPrec 3 f)

instance Weight a => Weight (Formula a) where
    weight = quantifiedWeight

instance FormulaElement a => FormulaElement (LinearChain Bond a) where
  toFormulaPrec p' (ChainItem i) = toFormulaPrec p' i
  toFormulaPrec p' (Chain i b is) = toFormulaPrec p' i . cons (bondToUnicode b) . toFormulaPrec p' is
  toFormulaMarkupPrec p' (ChainItem i) = toFormulaMarkupPrec p' i
  toFormulaMarkupPrec p' (Chain i b is) = toFormulaMarkupPrec p' i . (text (singleton (bondToUnicode b)) <>) . toFormulaMarkupPrec p' is

_positiveGen :: Gen Int
_positiveGen = (1+) . abs <$> arbitrary

instance Arbitrary a => Arbitrary (FormulaPart a) where
    arbitrary = arbitrary1

instance Arbitrary a => Arbitrary (Formula a) where
    arbitrary = arbitrary1

instance Arbitrary1 FormulaPart where
    liftArbitrary arb = oneof [FormulaItem <$> arb, (:*) <$> liftArbitrary arb <*> _positiveGen]

instance Arbitrary1 Formula where
    liftArbitrary arb = go
        where go = oneof [(:-) <$> arb' <*> go, FormulaPart <$> arb']
              arb' = liftArbitrary arb

instance Arbitrary2 LinearChain where
  liftArbitrary2 gb ga = go
      where go = oneof [ChainItem <$> ga, Chain <$> ga <*> gb <*> go]

instance Arbitrary bond => Arbitrary1 (LinearChain bond) where
  liftArbitrary = liftArbitrary2 arbitrary

instance (Arbitrary element, Arbitrary bond) => Arbitrary (LinearChain bond element) where
  arbitrary = arbitrary2
