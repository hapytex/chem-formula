{-# LANGUAGE FlexibleContexts, TemplateHaskellQuotes #-}

module Chemistry.Parser where

import Chemistry.Charge(Charged(Charged))
import Chemistry.Element(Element)
import Chemistry.Formula(Formula(FormulaPart, (:-)), FormulaPart(Element, (:*)), LinearChain(ChainItem, Chain))
import Chemistry.Bond(Bond, bondChars)

import Control.Applicative((<|>))
import Control.Arrow(first)

import Data.Char(digitToInt)
import Data.Data(Data)
import Data.Function((&))
import Data.Functor(($>))
import Data.Functor.Identity(Identity)
import Data.List(foldl', sortOn)

import Language.Haskell.TH(pprint)
import Language.Haskell.TH.Quote(QuasiQuoter(QuasiQuoter, quoteExp, quotePat, quoteType, quoteDec))
import Language.Haskell.TH.Syntax(Lift, Type(AppT, ConT), Q, dataToPatQ, lift, reportWarning)

import Text.Parsec(ParsecT, Stream, many1, option, optionMaybe, parserReturn, parserZero, runP)
import Text.Parsec.Char(digit, char)

_grouping :: Eq b => (a -> b) -> [a] -> [(b, [a])]
_grouping f = go
    where go [] = []
          go ~(x:xs) = (fx, x : ys) : go zs
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

elementParser :: Stream s m Char => ParsecT s u m Element
elementParser = parseTrie (map ((,) =<< show) [minBound ..])

chargedParser' :: Stream s m Char => ParsecT s u m a -> ParsecT s u m (Charged a)
chargedParser' el = Charged <$> el <*> charge

chargedParser :: Stream s m Char => ParsecT s u m (Charged Element)
chargedParser = chargedParser' elementParser

sign :: (Num a, Stream s m Char) => ParsecT s u m (a -> a)
sign = (id <$ char '+') <|> (negate <$ char '-')

charge :: Stream s m Char => ParsecT s u m Int
charge = option 0 ((&) <$> quantity <*> sign)

quantity :: Stream s m Char => ParsecT s u m Int
quantity = option 1 (foldl' ((. digitToInt) . (+) . (10 *)) 0 <$> many1 digit)

elementQuantityParser :: Stream s m Char => ParsecT s u m (Element, Int)
elementQuantityParser = (,) <$> elementParser <*> quantity

formulaParser :: Stream s m Char => ParsecT s u m (Formula Element)
formulaParser = formulaParser' elementParser

linearChainParser :: Stream s m Char => ParsecT s u m (LinearChain Bond (Formula Element))
linearChainParser = linearChainParser' formulaParser

linearChainParser' :: Stream s m Char => ParsecT s u m a -> ParsecT s u m (LinearChain Bond a)
linearChainParser' = linearChainParser'' bondParser

bondParser :: Stream s m Char => ParsecT s u m Bond
bondParser = foldr ((<|>) . uncurry (($>) . char)) parserZero bondChars

chargedFormulaParser :: Stream s m Char => ParsecT s u m (Formula (Charged Element))
chargedFormulaParser = formulaParser' chargedParser

quantity' :: Int -> a -> FormulaPart a
quantity' 1 = Element
quantity' n = (:* n) . FormulaPart . Element

formulaPartParser' :: Stream s m Char => ParsecT s u m a -> ParsecT s u m (FormulaPart a)
formulaPartParser' el = flip quantity' <$> el <*> quantity <|> ((:*) <$> (char '(' *> formulaParser' el <* char ')') <*> quantity)

formulaParser' :: Stream s m Char => ParsecT s u m a -> ParsecT s u m (Formula a)

formulaParser' el = go'
    where go' = go <$> formulaPartParser' el <*> optionMaybe (formulaParser' el)
          go fp Nothing = FormulaPart fp
          go fp ~(Just t) = fp :- t

linearChainParser'' :: Stream s m t => ParsecT s u m bond -> ParsecT s u m element -> ParsecT s u m (LinearChain bond element)
linearChainParser'' bo el = go'
    where go' = go <$> el <*> optionMaybe ((,) <$> bo <*> go')
          go fp Nothing = ChainItem fp
          go fp ~(Just be) = uncurry (Chain fp) be

_parsing :: Stream String Identity t => (a -> Q b) -> ParsecT String () Identity a -> String -> Q b
_parsing f p s = either (fail . show) f (runP p () s s)

_baseQQ :: (Data a, Lift a) => ParsecT String () Identity a -> Type -> QuasiQuoter
_baseQQ f typ = QuasiQuoter {
    quoteExp=_parsing lift f
  , quotePat=_parsing (dataToPatQ (const Nothing)) f
  , quoteType=const (reportWarning ("The type of the quasiquoter will always use the " <> pprint typ <> " type.") >> pure typ)
  , quoteDec=const (reportWarning "The use of this quasiquoter will not make any declarations." >> pure [])
  }

elqq :: QuasiQuoter
elqq = _baseQQ elementParser (ConT ''Element)

chelqq :: QuasiQuoter
chelqq = _baseQQ chargedParser (AppT (ConT ''Charged) (ConT ''Element))