{-# LANGUAGE DeriveDataTypeable, DeriveLift #-}

module Chemistry.Bond where

import Data.Data(Data)
import Data.Default(Default(def))
import Language.Haskell.TH.Syntax(Lift)

data Bond
  = BSingle
  | BDouble
  | BTriple
  | BQuadruple
  deriving (Data, Bounded, Enum, Eq, Lift, Ord, Read, Show)

instance Semigroup Bond where
  (<>) = max

instance Monoid Bond where
  mempty = BSingle

instance Default Bond where
  def = BSingle

bondToUnicode :: Bond -> Char
bondToUnicode BSingle = '-'
bondToUnicode BDouble = '='
bondToUnicode BTriple = '\8801'
bondToUnicode BQuadruple = '\8803'

bondFromUnicode :: Char -> Maybe Bond
bondFromUnicode '-' = Just BSingle  -- normal and SMILES
bondFromUnicode '=' = Just BDouble  -- normal and SMILES
bondFromUnicode '#' = Just BTriple  -- SMILES
bondFromUnicode '\8801' = Just BTriple -- normal
bondFromUnicode '$' = Just BQuadruple  -- SMILES
bondFromUnicode '\8803' = Just BQuadruple -- normal
bondFromUnicode _ = Nothing

bondChars :: [(Char, Bond)]
bondChars = [
    ('-',     BSingle)
  , ('=',     BDouble)
  , ('#',     BTriple)
  , ('\8801', BTriple)
  , ('$',     BQuadruple)
  , ('\8803', BQuadruple)
  ]
