{-# LANGUAGE FlexibleInstances, TupleSections #-}

{-|
Module      : Chemistry.Core
Description : A module that defines the main typeclasses and functions for parsing, presenting and rendering molecules.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module that defines the main typeclasses and functions for parsing, presenting and rendering molecules.
-}

module Chemistry.Core (
    -- * A collection where items can be multiplied
    QuantifiedElements(foldQuantified, listElements, listElements', listElementsCounter)
    -- * Rendering formulas as text and HTML
  , FormulaElement(toFormula, toFormulaColoured, toFormulaPrec, toFormulaPrecColoured, toFormulaMarkup, toFormulaMarkupPrec)
    -- * Items that have weight
  , Weight(weight), quantifiedWeight
    -- * Hill system
  , HillCompare(hillCompare)
    -- * Helper functions to render parenthesis
  , showParenText, showParenMarkup
  ) where

import Control.Applicative(liftA2)
import Control.Arrow(second)

import Data.Bool(bool)
import Data.Hashable(Hashable)
import Data.HashMap.Strict(HashMap, fromListWith)
import Data.Text(Text, cons, empty)

import Numeric.Units.Dimensional(DMass, Quantity, (*~), one)
import qualified Numeric.Units.Dimensional as D

import Text.Blaze(Markup, string, text)

-- | A helper function to wrap the text generator in parenthesis if the given condition is 'True', and simply returns the text generator otherwise.
showParenText
  :: Bool  -- ^ A 'Bool' that determines if we generate parenthesis.
  -> (Text -> Text)  -- ^ A text generator function that can be wrapped in parenthesis.
  -> Text -> Text  -- ^ The resulting text generator.
showParenText = bool id ((cons '(' .) . (. cons ')'))

-- | A helper function to wrap the markup generator in parenthesis if the given condition is 'True', and simply returns the markup generator otherwise.
showParenMarkup
  :: Bool  -- ^ A 'Bool' that determines if we generate parenthesis.
  -> (Markup -> Markup)  -- ^ A markup generator function that can be wrapped in parenthesis.
  -> Markup -> Markup  -- ^ The resulting markup generator.
showParenMarkup = bool id (((string "(" <>) .) . (. (<> string ")")))

-- | A typeclass to represent types that can be rendered as chemical formulas.
class FormulaElement a where
    -- | Render the given item as 'Text'.
    toFormula
      :: a  -- ^ The given item to render.
      -> Text  -- ^ A 'Text' object that represents the given chemical item.
    toFormula = flip (toFormulaPrec 0) empty

    toFormulaColoured
      :: a
      -> Text
    toFormulaColoured = flip (toFormulaPrecColoured 0) empty

    -- | Render the given chemical item with the /precedence/ value.
    toFormulaPrec
      :: Int  -- ^ The given precedence value.
      -> a -- ^ The chemical item to render.
      -> Text -- ^ The 'Text' object that is the "tail" of the 'Text' that will be generated.
      -> Text -- ^ The 'Text' that represents the chemical item appended with the given tail.
    toFormulaPrec _ = (<>) . toFormula

    toFormulaPrecColoured
      :: Int
      -> a
      -> Text
      -> Text
    toFormulaPrecColoured = toFormulaPrec

    -- | Render the given chemical item as 'Markup'.
    toFormulaMarkup
      :: a  -- ^ The given item to render.
      -> Markup -- ^ A 'Markup' object that represents the given chemical item.
    toFormulaMarkup = text . toFormula

    -- | Render the given chemical item with the /precedence/ value.
    toFormulaMarkupPrec
      :: Int  -- ^ The given precedence value.
      -> a -- ^ The chemical item to render.
      -> Markup -- ^ The given 'Markup' object that is the "tail" of the 'Markup' that will be generated.
      -> Markup -- ^ The 'Markup' that represents the chemical item appended with the given tail.
    toFormulaMarkupPrec _ = (<>) . toFormulaMarkup
    {-# MINIMAL toFormulaPrec | toFormula #-}

-- | A typeclass that represents that the given type has a /weight/.
class Weight a where
    -- | Calculate the weight of the given item.
    weight :: Floating b
      => a  -- ^ The given item to determine the weight from.
      -> Maybe (Quantity DMass b)  -- ^ The weight of the given item, given the item has a weight.
    weight = const Nothing
    {-# MINIMAL weight #-}


-- | Determine the weight of the given 'QuantifiedElements' object. This is implemented by summing and
-- multiplying the weights of the individual elements.
quantifiedWeight :: (QuantifiedElements f, Weight a, Floating b)
  => f a  -- ^ The given item to determine the weight from.
  -> Maybe (Quantity DMass b)  -- ^ The corresponding weight of the given checmical item.
quantifiedWeight = foldQuantified weight (liftA2 (D.+)) (fmap . (D.*) . (*~ one) . fromIntegral)

-- | A typeclass that specifies a collection where items can have a multiplication factor.
class QuantifiedElements f where
    -- | Fold a given 'QuantifiedElements' object by defining rules for mapping, appending, and multiplying.
    foldQuantified
      :: (a -> b)  -- ^ The given mapping function that maps items to a result type.
      -> (b -> b -> b)  -- ^ The given append function that is triggered when we have a combination of two 'QuantifiedElements' subexpressions.
      -> (Int -> b -> b)  -- ^ The given multiplication function when a certain subexpression occurs a given number of times.
      -> f a  -- ^ The 'QuantifiedElements' object that will be folded.
      -> b -- ^ The corresponding result of the fold operation.

    -- | List the elements in the 'QuantifiedElements' expression and append a given list. Items can occur /multiple/ times.
    listElements'
      :: f a  -- ^ The given 'QuantifiedElements' to determine the items from.
      -> [(a, Int)]  -- ^ The given list to append to the result.
      -> [(a, Int)]  -- ^ The result of determining the number of occurrences of the chemical items together with the given list.
    listElements' = (<>) . listElements

    -- | List the elements in the 'QuantifiedElements'. Items can occur /multiple/ times.
    listElements
      :: f a  -- ^ The given 'QuantifiedElements' to determine the item count from.
      -> [(a, Int)]  -- ^ A list of items with the number of times these occur. An item can occur /multiple/ times.
    listElements = foldQuantified (pure . (,1)) (<>) (map . second . (*))

    -- | Convert the given 'QuantifiedElements' to a 'HashMap' that maps each item to the number of times it is present.
    listElementsCounter :: (Eq a, Hashable a)
      => f a  -- ^ The given 'QuantifiedElements' to determine the item count from.
      -> HashMap a Int  -- ^ A 'HashMap' that maps the items to the number of times these occur.
    listElementsCounter = fromListWith (+) . listElements
    {-# MINIMAL foldQuantified #-}

-- | A typeclass that spans an order relation based on the rules of a /Hill system/.
class HillCompare a where
    -- | Compare the two given items according to the hill system.
    hillCompare
      :: a -- ^ The first item to compare with the Hill system.
      -> a -- ^ The second item to compare with the Hill system.
      -> Ordering -- ^ The result of the comparison with the Hill system.
    {-# MINIMAL hillCompare #-}
