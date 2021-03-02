{-# LANGUAGE DeriveDataTypeable, DeriveLift, DeriveTraversable, FlexibleInstances, OverloadedStrings, TypeFamilies #-}

{-|
Module      : Chemistry.Formula
Description : A module that defines datastructures to present formulas and linear chains.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module that defines datastructure to present formulas and linear chains.
-}

module Chemistry.Formula (
    -- * Representing formulas
    FormulaPart((:*), FormulaItem), Formula((:-), FormulaPart)
  , (.*), formulaToParts, fromElementList
    -- * Conversion to canonical forms
  , toMolecular, toHillFormula
    -- * Representing linear chain
  , LinearChain(Chain, ChainItem)
  , (.-), (.=), (.#), (.$)
  ) where

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

-- | A data type to specify the part of a formula. This allow to /multiply/ a 'Formula',
-- but /not/ concatenate.
data FormulaPart a
  = FormulaItem a  -- ^ Wrap an item to present it as a 'FormulaPart'.
  | (Formula a) :* Int  -- ^ Multiply a 'Formula' with a given number.
  deriving (Data, Eq, Foldable, Functor, Lift, Ord, Read, Show, Traversable)

-- | A data type to specify a formula. This allows to concatenate 'FormulaPart's together,
-- but not multiply these.
data Formula a
  = FormulaPart (FormulaPart a)  -- ^ Wrap a 'FormulaPart' to present it as a 'Formula'.
  | (FormulaPart a) :- (Formula a)  -- ^ Prepend a 'Formula' with a 'FormulaPart'.
  deriving (Data, Eq, Foldable, Functor, Lift, Ord, Read, Show, Traversable)

-- | A data type to present a chain of elements that are separated by (possibly different) bond.
data LinearChain bond element
  = ChainItem element  -- ^ Wrap a single element into a 'LinearChain'.
  | Chain element bond (LinearChain bond element) -- ^ Prepend an existing element with an element through a binding.
  deriving (Data, Eq, Foldable, Functor, Lift, Ord, Read, Show, Traversable)

-- | Multiply the given 'FormulaPart' with a given number to a new 'FormulaPart'.
(.*)
  :: FormulaPart a  -- ^ The original 'FormulaPart'.
  -> Int  -- ^ The given /multiplier/.
  -> FormulaPart a  -- ^ The given 'FormulaPart' multiplied with the given /multiplier/.
(.*) = (:*) . FormulaPart

-- | Create a 'LinearChain' by prepending an element with a /single/ bond to a 'LinearChain'.
(.-)
  :: element  -- ^ The element to prepend to the 'LinearChain'.
  -> LinearChain Bond element -- ^ The rest of the 'LinearChain'.
  -> LinearChain Bond element  -- ^ A 'LinearChain' where the given element is prepended through a /single/ bond.
(.-) = (`Chain` BSingle)

-- | Create a 'LinearChain' by prepending an element with a /double/ bond to a 'LinearChain'.
(.=)
  :: element  -- ^ The element to prepend to the 'LinearChain'.
  -> LinearChain Bond element -- ^ The rest of the 'LinearChain'.
  -> LinearChain Bond element  -- ^ A 'LinearChain' where the given element is prepended through a /double/ bond.
(.=) = (`Chain` BDouble)

-- | Create a 'LinearChain' by prepending an element with a /triple/ bond to a 'LinearChain'.
(.#)
  :: element  -- ^ The element to prepend to the 'LinearChain'.
  -> LinearChain Bond element -- ^ The rest of the 'LinearChain'.
  -> LinearChain Bond element  -- ^ A 'LinearChain' where the given element is prepended through a /triple/ bond.
(.#) = (`Chain` BTriple)

-- | Create a 'LinearChain' by prepending an element with a /quadruple/ bond to a 'LinearChain'.
(.$)
  :: element  -- ^ The element to prepend to the 'LinearChain'.
  -> LinearChain Bond element -- ^ The rest of the 'LinearChain'.
  -> LinearChain Bond element  -- ^ A 'LinearChain' where the given element is prepended through a /quadruple/ bond.
(.$) = (`Chain` BQuadruple)

-- | Convert a given formula to a 'NonEmpty' list of 'FormulaPart's.
formulaToParts
  :: Formula a  -- ^ The given formula to convert.
  -> NonEmpty (FormulaPart a)  -- ^ A 'NonEmpty' list of 'FormulaPart's the 'Formula' consists out of.
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

-- | Convert a given list of items with their quantity to a 'Formula'. This does /not/
-- allow to multiply a "formulapart" like @C2H4(OH)2@.
fromElementList
  :: [(a, Int)]  -- ^ The given list of elements with the corresponding quantity.
  -> Formula a -- ^ The corresponding 'Formula'.
fromElementList = fromList . map (uncurry ((:*) . FormulaPart . FormulaItem))

_processFormula :: (Eq a, Hashable a) => ([(a, Int)] -> [(a, Int)]) -> Formula a -> Formula a
_processFormula f = fromElementList . f . HM.toList . listElementsCounter

-- | Convert the given 'Formula' to the a 'Formula' that is a /molecular/ formula. In this
-- formula, each atom is denoted once with the total number of times it occurs.
toMolecular :: (Eq a, Hashable a) => Formula a -> Formula a
toMolecular = _processFormula id

-- | Convert the given 'Formula' to a 'Formula' that presents the molecule with the /Hill system/.
-- In this system /carbon/ is denoted first followed by /hydrogen/, etc.
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
