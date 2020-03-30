module Chemistry.Core where

import Data.Text(Text)

import Numeric.Units.Dimensional(DMass, Quantity)

class FormulaElement a where
    toFormula :: a -> Text
    weight :: Floating b => a -> Maybe (Quantity DMass b)
