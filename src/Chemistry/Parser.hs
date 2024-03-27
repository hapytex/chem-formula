{-# LANGUAGE FlexibleContexts, TemplateHaskellQuotes #-}
{-|
Module      : Chemistry.Parser
Description : A module that defines parsers to parse elements, formulas, bonds, etc.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module that defines parsers to parse elements, formulas, bonds, etc.
-}

module Chemistry.Parser (
    -- * Parsing elements
    elementParser
    -- * Parsing bonds
  , bondParser
    -- * Parsing charged items
  , chargedParser, chargedParser'
    -- * Parsing elements with quantities
  , quantityParser, elementQuantityParser
    -- * Parsing formulas
  , formulaParser, formulaParser', chargedFormulaParser
    -- * Parsing linear chains
  , linearChainParser, linearChainParser', linearChainParser'', chargedLinearChainParser
    -- * Quasiquoters
  , elqq, elqqq, chelqq, formulaqq, chainqq, chchainqq
  ) where

import Chemistry.Charge(Charged(Charged))
import Chemistry.Element(Element)
import Chemistry.Formula(Formula(FormulaPart, (:-)), FormulaPart(FormulaItem, (:*)), LinearChain(ChainItem, Chain))
import Chemistry.Bond(Bond, bondChars)

import Control.Applicative((<|>), liftA2)
import Control.Arrow(first)

import Data.Char(digitToInt)
import Data.Data(Data)
import Data.Function((&))
import Data.Functor(($>))
import Data.Functor.Identity(Identity)
import Data.List(foldl', sortOn)

import Language.Haskell.TH(pprint)
import Language.Haskell.TH.Quote(QuasiQuoter(QuasiQuoter, quoteExp, quotePat, quoteType, quoteDec))
import Language.Haskell.TH.Syntax(Lift, Type(AppT, ConT, TupleT), Q, dataToPatQ, lift, reportWarning)

import Text.Parsec(ParsecT, Stream, many1, option, optionMaybe, parserReturn, parserZero, runP)
import Text.Parsec.Char(digit, char)

_grouping :: Eq b => (a -> b) -> [a] -> [(b, [a])]
_grouping f = go
    where go [] = []
          go (x:xs) = (fx, x : ys) : go zs
              where ~(ys, zs) = span ((fx ==) . f) xs
                    fx = f x

_chomp :: [a] -> [a]
_chomp [] = []
_chomp (_:xs) = xs

parseTrie :: Stream s m Char => [(String, a)] -> ParsecT s u m a
parseTrie = _parseTrie . sortOn fst

_parseTrie :: Stream s m Char => [(String, a)] -> ParsecT s u m a
_parseTrie [] = parserZero
_parseTrie (("", x):ss) = _parseTrieRem ss <|> parserReturn x
_parseTrie ss = _parseTrieRem ss

_parseTrieLeg :: Stream s m Char => Char -> [(String, a)] -> ParsecT s u m a
_parseTrieLeg c is = char c *> _parseTrie (map (first _chomp) is)

_parseTrieRem :: Stream s m Char => [(String, a)] -> ParsecT s u m a
_parseTrieRem = foldr ((<|>) . uncurry _parseTrieLeg) parserZero . _grouping (head . fst)

-- | A parser that aims to convert text into an 'Element'.
elementParser :: Stream s m Char
  => ParsecT s u m Element  -- ^ The parser that reads to an 'Element'.
elementParser = parseTrie (map ((,) =<< show) [minBound ..])

-- | A parser that uses a parser, and reads that parser together with the charge.
-- The charge can be represented with a plus or minus followed by a number, or a sequence
-- of plusses and minusses where the number of characters determine the magnitude of
-- the charge.
chargedParser' :: Stream s m Char
  => ParsecT s u m a  -- ^ The given parser for the chemical item.
  -> ParsecT s u m (Charged a)  -- ^ A parser that will parse the chemical item followed by the charge.
chargedParser' el = Charged <$> el <*> charge

-- | A parser that parses charged items. The charge can be represented with a plus
-- or minus followed by a number, or a sequence of plusses and minusses where the
-- number of characters determine the magnitude of the charge.
chargedParser :: Stream s m Char
  => ParsecT s u m (Charged Element)  -- ^ The parser that parses charged 'Element's.
chargedParser = chargedParser' elementParser

sign :: (Num a, Stream s m Char) => ParsecT s u m (a -> a)
sign = (id <$ char '+') <|> (negate <$ char '-')

charge :: Stream s m Char => ParsecT s u m Int
charge = option 0 ((&) <$> quantity <*> sign)

quantity :: Stream s m Char => ParsecT s u m Int
quantity = option 1 (foldl' ((. digitToInt) . (+) . (10 *)) 0 <$> many1 digit)

-- | A function that is given a parser to parse an item and produces a parser
-- that parses the element with its quantity. If no quantity is given, one is used.
quantityParser :: Stream s m Char
  => ParsecT s u m a  -- ^ The element parser that is used.
  -> ParsecT s u m (a, Int)  -- ^ A parser that will parse the element together with an optional quantity.
quantityParser = flip (liftA2 (,)) quantity

-- | A parser that parses an 'Element' together with an optional quantity.
-- if no quantity is passed, one is used.
elementQuantityParser :: Stream s m Char
  => ParsecT s u m (Element, Int)  -- ^ The parser that parses a combination of an element and a quantity.
elementQuantityParser = quantityParser elementParser

-- | A parser that parses 'Formula's with 'Element's as items.
formulaParser :: Stream s m Char
  => ParsecT s u m (Formula Element) -- ^ A parser that parses 'Formula's with 'Element's as items.
formulaParser = formulaParser' elementParser

-- | A parser that parses a 'LinearChain' of 'Bond's and 'Formula's of 'Element's. This makes
-- use of the 'linearChainParser''', and uses other parsers to parse the bond and formula.
linearChainParser :: Stream s m Char
  => ParsecT s u m (LinearChain Bond (Formula Element)) -- ^ A parser that parses a 'LinearChain' of 'Bond's and 'Formula's of 'Element's
linearChainParser = linearChainParser' formulaParser

-- | A function that produces a parser that parses a 'LinearChain' that parses 'Bond's and makes use
-- of a given parser to parse the items in the linear chain.
linearChainParser' :: Stream s m Char
  => ParsecT s u m a  -- ^ The parser for the items that will be used.
  -> ParsecT s u m (LinearChain Bond a)  -- ^ A parser that parses 'LinearChain's with the given parser for the items.
linearChainParser' = linearChainParser'' bondParser

-- | A parser that parses the different types of bonds as defined by the 'bondChars'.
bondParser :: Stream s m Char
  => ParsecT s u m Bond  -- ^ A parser to parse the different types of bonds.
bondParser = foldr ((<|>) . uncurry (($>) . char)) parserZero bondChars

-- | A parser that parses 'Formula's with 'Element's that can contain a charge.
chargedFormulaParser :: Stream s m Char
  => ParsecT s u m (Formula (Charged Element))  -- ^ A parser that parses 'Formula's with 'Element's that can contain a charge.
chargedFormulaParser = formulaParser' chargedParser

-- | A parser that parses a 'LinearChain' of 'Bond's and 'Formula's of /charged/ 'Element's. This makes
-- use of the 'linearChainParser''', and uses other parsers to parse the bond and formula of charged elements.
chargedLinearChainParser :: Stream s m Char
  => ParsecT s u m (LinearChain Bond (Formula (Charged Element))) -- ^ A parser that parses a 'LinearChain' of 'Bond's and /charged/ 'Formula's of 'Element's
chargedLinearChainParser = linearChainParser' chargedFormulaParser

quantity' :: Int -> a -> FormulaPart a
quantity' 1 = FormulaItem
quantity' n = (:* n) . FormulaPart . FormulaItem

formulaPartParser' :: Stream s m Char => ParsecT s u m a -> ParsecT s u m (FormulaPart a)
formulaPartParser' el = flip quantity' <$> el <*> quantity <|> ((:*) <$> (char '(' *> formulaParser' el <* char ')') <*> quantity)

-- | A parser that will for a given element parser parse a formula which is a combination of concatenations and multiplications.
formulaParser' :: Stream s m Char
  => ParsecT s u m a  -- ^ The given parser that parses an element.
  -> ParsecT s u m (Formula a)  -- ^ The resulting parser that parses formulas with elements parsed by the given element parser.
formulaParser' el = go'
    where go' = go <$> formulaPartParser' el <*> optionMaybe (formulaParser' el)
          go fp Nothing = FormulaPart fp
          go fp (Just t) = fp :- t

-- | A function that produces a parser that parses a 'LinearChain' that makes
-- use of given parsers to parse the bonds and the items in the linear chain.
linearChainParser'' :: Stream s m t
  => ParsecT s u m bond  -- ^ The given parser to parse the bonds.
  -> ParsecT s u m element  -- ^ The given parser to parse the items.
  -> ParsecT s u m (LinearChain bond element) -- ^ A parser that parses 'LinearChain's for the given bond and item parsers.
linearChainParser'' bo el = go'
    where go' = go <$> el <*> optionMaybe ((,) <$> bo <*> go')
          go fp Nothing = ChainItem fp
          go fp (Just be) = uncurry (Chain fp) be

_parsing :: (a -> Q b) -> ParsecT String () Identity a -> String -> Q b
_parsing f p s = either (fail . show) f (runP p () s s)

_baseQQ :: (Data a, Lift a) => ParsecT String () Identity a -> Type -> QuasiQuoter
_baseQQ f typ = QuasiQuoter {
    quoteExp=_parsing lift f
  , quotePat=_parsing (dataToPatQ (const Nothing)) f
  , quoteType=const (reportWarning ("The type of the quasiquoter will always use the " <> pprint typ <> " type.") >> pure typ)
  , quoteDec=const (reportWarning "The use of this quasiquoter will not make any declarations." >> pure [])
  }

-- | A quasiquoter that can parse chemical 'Element's. The quasiquoter can be used for expressions and patterns.
-- If it is used as a type signature, it will be replaced with the 'Element' type.
elqq :: QuasiQuoter  -- ^ A quasiquoter to parse 'Element's.
elqq = _baseQQ elementParser (ConT ''Element)

-- | A quasiquoter that can parse chemical 'Element's with an optional quantity. The quasiquoter can be used for expressions and patterns.
-- If it is used as a type signature, it will be replaced with a 2-tuple type with an 'Element' and an 'Int'.
elqqq :: QuasiQuoter -- ^ A quasiquoter to parse a combination of an 'Element' and its quantity.
elqqq = _baseQQ elementQuantityParser (AppT (AppT (TupleT 2) (ConT ''Element)) (ConT ''Int))

-- | A quasiquoter that can parse 'Charged' chemical 'Element's with an optional quantity. The quasiquoter can be used for expressions and patterns.
-- If it is used as a type signature, it will be replaced with a 'Charged' 'Element'.
chelqq :: QuasiQuoter  -- ^ A quasiquoter that parses /charged/ 'Element's.
chelqq = _baseQQ chargedParser (AppT (ConT ''Charged) (ConT ''Element))

-- | A quasiquoter that can parse 'Formula's of 'Element's. The quasiquoter can be used for expressions and patterns.
-- If it is used as a type signature, it will be replaced with a 'Formula' 'Element'.
formulaqq :: QuasiQuoter  -- ^ A quasiquoter that parses 'Formula's of 'Element's.
formulaqq = _baseQQ formulaParser (AppT (ConT ''Formula) (ConT ''Element))

-- | A quasiquoter that can parse a 'LinearChain' of 'Bond's and 'Element's.
-- The quasiquoter can be used for expressions and patterns. If it is used
-- as a type signature, it will be replaced with a 'LinearChain' of 'Bond's and 'Element's.
chainqq :: QuasiQuoter  -- ^ A quasiquoter that parses 'LinearChain's of 'Bond's and 'Formula's of 'Element's.
chainqq = _baseQQ linearChainParser (AppT (AppT (ConT ''LinearChain) (ConT ''Bond)) (AppT (ConT ''Formula) (ConT ''Element)))

-- | A quasiquoter that can parse a 'LinearChain' of 'Bond's and 'Charged' 'Element's.
-- The quasiquoter can be used for expressions and patterns. If it is used
-- as a type signature, it will be replaced with a 'LinearChain' of 'Bond's and 'Charged' 'Element's.
chchainqq :: QuasiQuoter -- A quasiquoter that parses 'LinearChain's of 'Bond's and 'Formula's of /charged/ 'Element's.
chchainqq = _baseQQ chargedLinearChainParser (AppT (AppT (ConT ''LinearChain) (ConT ''Bond)) (AppT (ConT ''Formula) (AppT (ConT ''Charged) (ConT ''Element))))
