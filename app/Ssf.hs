{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Ssf where

import Ast

data Liter =
      PredicateSymbol Symbol
    | NegPredicateSymbol Symbol

newtype Disjunct = Disjunct [Liter]

newtype CNF = CNF [Disjunct]

newtype UniversalQuant = UniversalQuant Var

data SSF = SSF {quants :: [UniversalQuant], matrix :: CNF}
