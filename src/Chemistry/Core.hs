{-# LANGUAGE FlexibleInstances, TupleSections #-}

{-|
Module      : Chemistry.Core
Description : A module that defines the main typeclasses and functions for parsing, presenting and rendering molecules.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module that defines the main typeclasses and functions for parsing, presenting and rendering molecules.
-}

module Chemistry.Core where

import Control.Applicative(liftA2)
import Control.Arrow(second)

import Data.Bool(bool)
import Data.Hashable(Hashable)
import Data.HashMap.Strict(HashMap, fromListWith)
import Data.Text(Text, cons, empty)

import Numeric.Units.Dimensional(DMass, Quantity, (*~), one)
import qualified Numeric.Units.Dimensional as D

import Text.Blaze(Markup, string, text)

showParen' :: Bool -> (Text -> Text) -> Text -> Text
showParen' = bool id ((cons '(' .) . (. cons ')'))

showParenMarkup' :: Bool -> (Markup -> Markup) -> Markup -> Markup
showParenMarkup' = bool id (((string "(" <>) .) . (. (<> string ")")))

class FormulaElement a where
    toFormula :: a -> Text
    toFormula = flip (toFormulaPrec 0) empty
    toFormulaPrec :: Int -> a -> Text -> Text
    toFormulaPrec _ = (<>) . toFormula
    toFormulaMarkup :: a -> Markup
    toFormulaMarkup = text . toFormula
    toFormulaMarkupPrec :: Int -> a -> Markup -> Markup
    toFormulaMarkupPrec _ = (<>) . toFormulaMarkup
    {-# MINIMAL toFormulaPrec | toFormula #-}

class Weight a where
    weight :: Floating b => a -> Maybe (Quantity DMass b)
    weight = const Nothing
    {-# MINIMAL weight #-}


quantifiedWeight :: (QuantifiedElements f, Weight a, Floating b) => f a -> Maybe (Quantity DMass b)
quantifiedWeight = foldQuantified weight (liftA2 (D.+)) (fmap . (D.*) . (*~ one) . fromIntegral)

class QuantifiedElements f where
    foldQuantified :: (a -> b) -> (b -> b -> b) -> (Int -> b -> b) -> f a -> b
    listElements' :: f a -> [(a, Int)] -> [(a, Int)]
    listElements' = (<>) . listElements
    listElements :: f a -> [(a, Int)]
    listElements = foldQuantified (pure . (,1)) (<>) (map . second . (*))
    listElementsCounter :: (Eq a, Hashable a) => f a -> HashMap a Int
    listElementsCounter = fromListWith (+) . listElements
    {-# MINIMAL foldQuantified #-}

class HillCompare a where
    hillCompare :: a -> a -> Ordering
    {-# MINIMAL hillCompare #-}
