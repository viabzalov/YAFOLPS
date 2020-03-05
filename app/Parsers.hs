{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Parsers where

import Text.Parsec
import Ast

type Parser = Parsec String ()

validChar :: Parser Char
validChar = letter <|> digit <|> char '_'

parseVar :: Parser String
parseVar = (:) <$> upper <*> many validChar

parseSymbol :: Parser Symbol
parseSymbol = Symbol <$> ((:) <$> lower <*> many validChar) <*> choice [
        char '(' *> (parseTerm `sepBy` char ',') <* char ')',
        pure []
    ]

parseTerm :: Parser Term
parseTerm =
    choice [
        Variable <$> parseVar,
        FunctionSymbol <$> parseSymbol
    ]

parseFormula' :: Parser Formula
parseFormula' =
    choice [
        char '(' *> parseFormula <* char ')',
        char '!' *> (Forall <$> parseVar <*> parseFormula'),
        char '?' *> (Exist <$> parseVar <*> parseFormula'),
        char '^' *> pure Top,
        char '_' *> pure Bottom,
        char '-' *> (Neg <$> parseFormula'),
        PredicateSymbol <$> parseSymbol
    ]

parseFormula :: Parser Formula
parseFormula = ((parseFormula'
    `chainl1` (char '&' *> pure Conj))
    `chainl1` (char '|' *> pure Disj))
    `chainr1` (char '>' *> pure Impl)
