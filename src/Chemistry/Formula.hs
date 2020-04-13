{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

module Chemistry.Formula where

import Chemistry.Core(FormulaElement(toFormula, weight))
import Chemistry.Element(Element)

import Control.Applicative(liftA2)

import Data.Char.Small(asSub)
import Data.HashMap.Strict(HashMap, fromListWith)
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Text(cons, snoc)

import GHC.Exts(IsList(Item, fromList, toList))

import Numeric.Units.Dimensional(DMass, Quantity, (*~), one)
import qualified Numeric.Units.Dimensional as D
import Numeric.Units.Dimensional.NonSI(dalton)

import Test.QuickCheck(Gen, oneof)
import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary))

infix 8 :*
infixr 7 :-

data FormulaPart
    = Element Element
    | Formula :* Int
    deriving (Eq, Ord, Read, Show)

data Formula
    = FormulaPart FormulaPart
    | FormulaPart :- Formula
    deriving (Eq, Ord, Read, Show)

(.*) :: FormulaPart -> Int -> FormulaPart
(.*) = (:*) . FormulaPart

formulaToParts :: Formula -> NonEmpty FormulaPart
formulaToParts (FormulaPart p) = p :| []
formulaToParts (p :- f) = p :| toList f

instance IsList Formula where
    type Item Formula = FormulaPart
    fromList [] = error "A formula contains at least one element"
    fromList (fp:fps) = go fps fp
        where go [] = FormulaPart
              go (x:xs) = (:- go xs x)
    toList (FormulaPart p) = [p]
    toList (p :- f) = p : toList f

instance Semigroup Formula where
    (<>) (FormulaPart p) = (p :-)
    (<>) (f :- p) = (f :-) . (p <>)

_listElements :: Formula -> [(Element, Int)]
_listElements = go 1 []
    where go n = go'
              where go' tl (p :- f) = go'' (go' tl f) p
                    go' tl (FormulaPart p) = go'' tl p
                    go'' tl (Element e) = (e, n) : tl
                    go'' tl (f :* n') = go (n*n') tl f
                    

_listElements' :: Formula -> HashMap Element Int
_listElements' = fromListWith (+) . _listElements

molecularMass :: Floating a => Formula -> Maybe (Quantity DMass a)
molecularMass = foldr (liftA2 (D.+) . (\(e, n) -> ((fromIntegral n *~ one) D.*) <$> weight e)) (Just (0 *~ dalton)) . _listElements

toMolecular :: Formula -> Formula
toMolecular = fromList . map (uncurry ((:*) . FormulaPart . Element)) . HM.toList . _listElements'
-- toEmpirical :: Formula -> Formula

instance FormulaElement FormulaPart where
    toFormula (Element e) = toFormula e
    toFormula (f :* n) = go f <> asSub n
        where go (FormulaPart (Element e)) = toFormula e
              go fp = cons '(' (snoc (toFormula fp) ')')
    weight (f :* n) = ((fromIntegral n *~ one) D.*) <$> weight f
    weight (Element e) = weight e

instance FormulaElement Formula where
    toFormula (FormulaPart p) = toFormula p
    toFormula (p :- f) = toFormula p <> toFormula f
    weight = molecularMass

_positiveGen :: Gen Int
_positiveGen = (1+) . abs <$> arbitrary

instance Arbitrary FormulaPart where
    arbitrary = oneof [ Element <$> arbitrary, (:*) <$> arbitrary <*> _positiveGen ]

instance Arbitrary Formula where
    arbitrary = oneof [ (:-) <$> arbitrary <*> arbitrary, FormulaPart <$> arbitrary ]
