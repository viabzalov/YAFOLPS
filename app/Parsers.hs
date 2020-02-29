{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Parsers where

import Text.Parsec
import Ast

type Parser = Parsec String ()

parseVar :: Parser Var
parseVar = read <$> (many1 digit)

parseFS :: Parser FS
parseFS = FS <$> ((:) <$> lower <*> many letter) <*> choice [
        char '(' *> (parseTerm `sepBy` char ',') <* char ')',
        pure []
    ]

parsePS :: Parser PS
parsePS =
    PS <$>
    -- name
    (
        (:) <$> upper <*> many letter
    ) <*>
    -- terms
    choice [
        char '(' *> (parseTerm `sepBy` char ',') <* char ')',
        pure []
    ]

parseTerm :: Parser Term
parseTerm =
    choice [
        Variable <$> parseVar,
        FunctionSymbol <$> parseFS
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
        PredicateSymbol <$> parsePS
    ]

parseFormula :: Parser Formula
parseFormula = ((parseFormula'
    `chainl1` (char '&' *> pure Conj))
    `chainl1` (char '|' *> pure Disj))
    `chainr1` (char '>' *> pure Impl)
