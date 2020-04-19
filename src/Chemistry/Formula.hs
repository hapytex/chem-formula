{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

module Chemistry.Formula where

import Chemistry.Core(FormulaElement(toFormula, weight))

import Control.Applicative(liftA2)

import Data.Char.Small(asSub)
import Data.Hashable(Hashable)
import Data.HashMap.Strict(HashMap, fromListWith)
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Text(cons, snoc)

import GHC.Exts(IsList(Item, fromList, toList))

import Numeric.Units.Dimensional(DMass, Quantity, (*~), one)
import qualified Numeric.Units.Dimensional as D
import Numeric.Units.Dimensional.NonSI(dalton)

import Test.QuickCheck(Gen, oneof)
import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), Arbitrary1(liftArbitrary), arbitrary1)

infix 8 :*
infixr 7 :-

data FormulaPart a
    = Element a
    | (Formula a) :* Int
    deriving (Eq, Ord, Read, Show)

data Formula a
    = FormulaPart (FormulaPart a)
    | (FormulaPart a) :- (Formula a)
    deriving (Eq, Ord, Read, Show)

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

molecularMass :: (Floating a, FormulaElement b) => Formula b -> Maybe (Quantity DMass a)
molecularMass = foldr (liftA2 (D.+) . (\(e, n) -> ((fromIntegral n *~ one) D.*) <$> weight e)) (Just (0 *~ dalton)) . _listElements

toMolecular :: (Eq a, Hashable a) => Formula a -> Formula a
toMolecular = fromList . map (uncurry ((:*) . FormulaPart . Element)) . HM.toList . _listElements'
-- toEmpirical :: Formula -> Formula

instance FormulaElement a => FormulaElement (FormulaPart a) where
    toFormula (Element e) = toFormula e
    toFormula (f :* n) = go f <> asSub n
        where go (FormulaPart (Element e)) = toFormula e
              go fp = cons '(' (snoc (toFormula fp) ')')
    weight (f :* n) = ((fromIntegral n *~ one) D.*) <$> weight f
    weight (Element e) = weight e

instance FormulaElement a => FormulaElement (Formula a) where
    toFormula (FormulaPart p) = toFormula p
    toFormula (p :- f) = toFormula p <> toFormula f
    weight = molecularMass

_positiveGen :: Gen Int
_positiveGen = (1+) . abs <$> arbitrary

instance Arbitrary a => Arbitrary (FormulaPart a) where
    arbitrary = arbitrary1 -- oneof [ Element <$> arbitrary, (:*) <$> arbitrary <*> _positiveGen ]

instance Arbitrary a => Arbitrary (Formula a) where
    arbitrary = arbitrary1 -- oneof [ (:-) <$> arbitrary <*> arbitrary, FormulaPart <$> arbitrary ]

instance Arbitrary1 FormulaPart where
    liftArbitrary arb = oneof [Element <$> arb, (:*) <$> liftArbitrary arb <*> _positiveGen]

instance Arbitrary1 Formula where
    liftArbitrary arb = go
        where go = oneof [(:-) <$> arb' <*> go, FormulaPart <$> arb']
              arb' = liftArbitrary arb
