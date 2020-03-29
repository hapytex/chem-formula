module Chemistry.Formula where

import Chemistry.Element

import Control.Applicative(liftA2)

import Data.Scientific(Scientific)

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

molecularMass :: Formula -> Maybe Scientific
molecularMass = foldr (liftA2 (+) . (\(e, n) -> (fromIntegral n *) <$> atomicWeight e)) (Just 0) . _listElements

--toMolecular :: Formula -> Formula
--toMolecular _ = _
-- toEmpirical :: Formula -> Formula
