module Chemistry.Core where

import Data.Bool(bool)
import Data.Text(Text, cons, empty)

import Numeric.Units.Dimensional(DMass, Quantity)

showParen' :: Bool -> (Text -> Text) -> Text -> Text
showParen' = bool id ((cons '(' .) . (. cons ')'))

class FormulaElement a where
    toFormula :: a -> Text
    toFormula = flip (toFormulaPrec 0) empty
    toFormulaPrec :: Int -> a -> Text -> Text
    toFormulaPrec _ = (<>) . toFormula
    {-# MINIMAL toFormulaPrec | toFormula #-}

class Weight a where
    weight :: Floating b => a -> Maybe (Quantity DMass b)
    weight = const Nothing
    {-# MINIMAL weight #-}

class HillCompare a where
    hillCompare :: a -> a -> Ordering
    {-# MINIMAL hillCompare #-}
