module Chemistry.Core where

import Data.Text(Text, cons, empty)

import Numeric.Units.Dimensional(DMass, Quantity)

showParen' :: Bool -> (Text -> Text) -> Text -> Text
showParen' True f  = cons '(' . f . cons ')'
showParen' False f = f

class FormulaElement a where
    toFormula :: a -> Text
    toFormula = flip (toFormulaPrec 0) empty
    toFormulaPrec :: Int -> a -> Text -> Text
    toFormulaPrec _ = (<>) . toFormula
    weight :: Floating b => a -> Maybe (Quantity DMass b)
    {-# MINIMAL (toFormulaPrec | toFormula), weight #-}
