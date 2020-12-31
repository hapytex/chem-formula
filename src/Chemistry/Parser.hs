{-# LANGUAGE FlexibleContexts #-}

module Chemistry.Parser where

import Chemistry.Element(Element)
import Chemistry.Formula(Formula(FormulaPart, (:-)), FormulaPart(Element, (:*)), fromElementList)

import Control.Applicative((<|>))
import Control.Arrow(first)

import Data.Char(digitToInt)
import Data.List(foldl', sortOn)

import Text.Parsec(ParsecT, Stream, many1, option, optionMaybe, parserReturn, parserZero, try)
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
_parseTrieRem = foldr (<|>) parserZero . map (uncurry _parseTrieLeg) . _grouping (head . fst)

elementParser :: Stream s m Char => ParsecT s u m Element
elementParser = parseTrie (map ((,) =<< show) [minBound ..])

quantity :: Stream s m Char => ParsecT s u m Int
quantity = option 1 (foldl' ((. digitToInt) . (+) . (10 *)) 0 <$> many1 digit)

elementQuantityParser :: Stream s m Char => ParsecT s u m (Element, Int)
elementQuantityParser = (,) <$> elementParser <*> quantity

formulaParser :: Stream s m Char => ParsecT s u m (Formula Element)
formulaParser = formulaParser' elementParser

quantity' :: Int -> a -> FormulaPart a
quantity' 1 = Element
quantity' n = (:* n) . FormulaPart . Element

formulaPartParser' :: Stream s m Char => ParsecT s u m a -> ParsecT s u m (FormulaPart a)
formulaPartParser' el = (flip quantity') <$> el <*> quantity <|> ((:*) <$> (char '(' *> formulaParser' el <* char ')') <*> quantity)

formulaParser' :: Stream s m Char => ParsecT s u m a -> ParsecT s u m (Formula a)
formulaParser' el = go <$> formulaPartParser' el <*> optionMaybe (formulaParser' el)
    where go fp Nothing = FormulaPart fp
          go fp (Just t) = fp :- t
