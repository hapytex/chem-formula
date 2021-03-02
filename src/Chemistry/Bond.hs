{-# LANGUAGE DeriveDataTypeable, DeriveLift #-}

{-|
Module      : Chemistry.Bond
Description : A module that defines the different types of chemical bonds.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module that defines the different types of chemical bonds together with conversions from and to corresponding unicode characters.
-}

module Chemistry.Bond (
    -- * Types of bonds
    Bond(BSingle, BDouble, BTriple, BQuadruple)
    -- * Rendering and parsing bonds
  , bondToUnicode, bondFromUnicode, bondChars
  ) where

import Data.Data(Data)
import Data.Default(Default(def))
import Language.Haskell.TH.Syntax(Lift)

-- | A datatype that defines the different types of bonds between atoms.
-- There are single, double, triple and quadruple bonds. 'Bond' is an instance
-- of 'Monoid' where we implement it by picking the maximum bond. The neutral
-- element is thus a single bond.
data Bond
  = BSingle -- ^ A /single/ bond.
  | BDouble -- ^ A /double/ bond.
  | BTriple -- ^ A /triple/ bond.
  | BQuadruple -- ^ A /quadruple/ bond.
  deriving (Data, Bounded, Enum, Eq, Lift, Ord, Read, Show)

instance Semigroup Bond where
  (<>) = max

instance Monoid Bond where
  mempty = BSingle

instance Default Bond where
  def = BSingle

-- | Convert the given 'Bond' type to a unicode character to format that binding.
bondToUnicode :: Bond -> Char
bondToUnicode BSingle = '-'
bondToUnicode BDouble = '='
bondToUnicode BTriple = '\8801'
bondToUnicode BQuadruple = '\8803'

-- | Parse the given 'Char'acter to the corresponding 'Bond'. It makes use
-- of the characters defined in 'bondToUnicode' as well as characters defined
-- by the /SMILES/ standard.
bondFromUnicode :: Char -> Maybe Bond
bondFromUnicode '-' = Just BSingle  -- normal and SMILES
bondFromUnicode '=' = Just BDouble  -- normal and SMILES
bondFromUnicode '#' = Just BTriple  -- SMILES
bondFromUnicode '\8801' = Just BTriple -- normal
bondFromUnicode '$' = Just BQuadruple  -- SMILES
bondFromUnicode '\8803' = Just BQuadruple -- normal
bondFromUnicode _ = Nothing

-- | A list of 2-tuples where the left item defines a 'Char'acter and the right
-- item the corresponding 'Bond'. This is used for parsing purposes.
bondChars :: [(Char, Bond)]
bondChars = [
    ('-',     BSingle)
  , ('=',     BDouble)
  , ('#',     BTriple)
  , ('\8801', BTriple)
  , ('$',     BQuadruple)
  , ('\8803', BQuadruple)
  ]
