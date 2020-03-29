module Chemistry.Formula where

import Chemistry.Element

import Control.Applicative(liftA2)

import Data.HashMap.Strict(HashMap, fromListWith, toList)

import Numeric.Units.Dimensional(DMass, Quantity, (*~), one)
import qualified Numeric.Units.Dimensional as D
import Numeric.Units.Dimensional.NonSI(dalton)

data Formula
    = Element Element
    | Combine Formula Formula
    | Times Formula Int

instance Semigroup Formula where
    (<>) = Combine

_listElements :: Formula -> [(Element, Int)]
_listElements = go 1 []
    where go n = go'
              where go' tl (Element e) = (e, n) : tl
                    go' tl (Times f n') = go (n'*n) tl f
                    go' tl (Combine f1 f2) = go' (go' tl f2) f1

_listElements' :: Formula -> HashMap Element Int
_listElements' = fromListWith (+) . _listElements

molecularMass :: Floating a => Formula -> Maybe (Quantity DMass a)
molecularMass = foldr (liftA2 (D.+) . (\(e, n) -> ((fromIntegral n *~ one) D.*) <$> atomicWeight e)) (Just (0 *~ dalton)) . _listElements

toMolecular :: Formula -> Formula
toMolecular = foldr1 (<>) . map (uncurry (Times . Element)) . toList . _listElements'
-- toEmpirical :: Formula -> Formula
