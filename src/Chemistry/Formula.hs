{-# LANGUAGE DeriveTraversable, FlexibleInstances, OverloadedStrings, TypeFamilies #-}

module Chemistry.Formula where

import Chemistry.Bond(Bond, bondToUnicode)
import Chemistry.Core(FormulaElement(toFormulaPrec), HillCompare(hillCompare), Weight(weight), showParen')

import Control.Applicative(liftA2)

import Data.Char.Small(asSub)
import Data.Function(on)
import Data.Hashable(Hashable)
import Data.HashMap.Strict(HashMap, fromListWith)
import qualified Data.HashMap.Strict as HM
import Data.List(sortBy)
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Text(cons)

import GHC.Exts(IsList(Item, fromList, toList))

import Numeric.Units.Dimensional(DMass, Quantity, (*~), one)
import qualified Numeric.Units.Dimensional as D
import Numeric.Units.Dimensional.NonSI(dalton)

import Test.QuickCheck(Gen, oneof)
import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), Arbitrary1(liftArbitrary), Arbitrary2(liftArbitrary2), arbitrary1, arbitrary2)

infix 8 :*
infixr 7 :-

data FormulaPart a
  = Element a
  | (Formula a) :* Int
  deriving (Eq, Foldable, Functor, Ord, Read, Show, Traversable)

data Formula a
  = FormulaPart (FormulaPart a)
  | (FormulaPart a) :- (Formula a)
  deriving (Eq, Foldable, Functor, Ord, Read, Show, Traversable)

data LinearChain bond element
  = ChainItem element
  | Chain element bond (LinearChain bond element)
  deriving (Eq, Foldable, Functor, Ord, Read, Show)

(.*) :: FormulaPart a -> Int -> FormulaPart a
(.*) = (:*) . FormulaPart

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

instance Monoid bond => IsList (LinearChain bond element) where
    type Item (LinearChain bond element) = element
    fromList [] = error "A linear chain contains at least one element"
    fromList (e:es) = go es e
        where go [] = ChainItem
              go (c:cs) = (`h` (go cs c))
              h = (`Chain` mempty)
    toList (ChainItem e) = [e]
    toList (Chain e _ es) = e : toList es

instance Semigroup (Formula a) where
    (<>) (FormulaPart p) = (p :-)
    (<>) (f :- p) = (f :-) . (p <>)

_listElements :: Formula a -> [(a, Int)]
_listElements = go 1 []
    where go n = go'
              where go' tl (p :- f) = go'' (go' tl f) p
                    go' tl (FormulaPart p) = go'' tl p
                    go'' tl (Element e) = (e, n) : tl
                    go'' tl (f :* n') = go (n*n') tl f


_listElements' :: (Eq a, Hashable a) => Formula a -> HashMap a Int
_listElements' = fromListWith (+) . _listElements

fromElementList :: [(a, Int)] -> Formula a
fromElementList = fromList . map (uncurry ((:*) . FormulaPart . Element))

molecularMass :: (Floating a, Weight b) => Formula b -> Maybe (Quantity DMass a)
molecularMass = foldr (liftA2 (D.+) . (\(e, n) -> ((fromIntegral n *~ one) D.*) <$> weight e)) (Just (0 *~ dalton)) . _listElements

_processFormula :: (Eq a, Hashable a) => ([(a, Int)] -> [(a, Int)]) -> Formula a -> Formula a
_processFormula f = fromElementList . f . HM.toList . _listElements'

toMolecular :: (Eq a, Hashable a) => Formula a -> Formula a
toMolecular = _processFormula id

toHillFormula :: (Eq a, Hashable a, HillCompare a) => Formula a -> Formula a
toHillFormula = _processFormula (sortBy (hillCompare `on` fst))

instance FormulaElement a => FormulaElement (FormulaPart a) where
    toFormulaPrec p (Element e) = toFormulaPrec p e
    toFormulaPrec p (f :* 1) = showParen' (p >= 5) (toFormulaPrec 5 f)
    toFormulaPrec p (f :* n) = showParen' (p >= 5) (toFormulaPrec 5 f . (asSub n <>))

instance Weight a => Weight (FormulaPart a) where
    weight (f :* n) = ((fromIntegral n *~ one) D.*) <$> weight f
    weight (Element e) = weight e

instance FormulaElement a => FormulaElement (Formula a) where
    toFormulaPrec p' (FormulaPart p) = toFormulaPrec p' p
    toFormulaPrec p' (p :- f) = showParen' (p' >= 4) (toFormulaPrec 3 p . toFormulaPrec 3 f)

instance FormulaElement a => FormulaElement (LinearChain Bond a) where
  toFormulaPrec p' (ChainItem i) = toFormulaPrec p' i
  toFormulaPrec p' (Chain i b is) = toFormulaPrec p' i . cons (bondToUnicode b) . toFormulaPrec p' is

instance Weight a => Weight (Formula a) where
    weight = molecularMass

_positiveGen :: Gen Int
_positiveGen = (1+) . abs <$> arbitrary

instance Arbitrary a => Arbitrary (FormulaPart a) where
    arbitrary = arbitrary1

instance Arbitrary a => Arbitrary (Formula a) where
    arbitrary = arbitrary1

instance Arbitrary1 FormulaPart where
    liftArbitrary arb = oneof [Element <$> arb, (:*) <$> liftArbitrary arb <*> _positiveGen]

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
