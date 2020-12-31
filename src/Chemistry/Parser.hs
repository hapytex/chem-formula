{-# LANGUAGE FlexibleContexts #-}

module Chemistry.Parser where

import Chemistry.Element(Element)
import Chemistry.Formula(Formula, fromElementList)

import Control.Applicative((<|>))
import Control.Arrow(first)

import Data.Char(digitToInt)
import Data.List(foldl', sortOn)

import Text.Parsec(ParsecT, Stream, many1, option, parserReturn, parserZero)
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
quantity = foldl' ((. digitToInt) . (+) . (10 *)) 0 <$> many1 digit

elementQuantityParser :: Stream s m Char => ParsecT s u m (Element, Int)
elementQuantityParser = (,) <$> elementParser <*> option 1 quantity

-- TODO: parsing brackets, etc.
formulaParser :: Stream s m Char => ParsecT s u m (Formula Element)
formulaParser = fromElementList <$> many1 elementQuantityParser