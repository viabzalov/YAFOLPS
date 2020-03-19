{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Ssf where

import           Ast

data Literal =
      PS {getPS :: Symbol}
    | NegPS {getPS :: Symbol}
    deriving (Eq, Ord, Show)

isPositive :: Literal -> Bool
isPositive (PS _) = True
isPositive (NegPS _) = False

type Disjunct = [Literal]

type Conjunct = [Literal]

type DNF = [Conjunct]

type CNF = [Disjunct]

type UniversalQuant = String

data SSF = SSF {quants :: [UniversalQuant], matrix :: CNF} deriving (Eq, Ord, Show)
