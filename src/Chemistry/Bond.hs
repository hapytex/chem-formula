module Chemistry.Bond where

data Bond
  = Single
  | Double
  | Triple
  | Quadruple
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

toUnicode :: Bond -> Char
toUnicode Single = '-'
toUnicode Double = '='
toUnicode Triple = '\x2261'
toUnicode Quadruple = '\x2263'
