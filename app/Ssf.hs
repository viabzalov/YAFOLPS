{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Ssf where

import           Ast

data Liter =
      PS {getPS :: Symbol}
    | NegPS {getPS :: Symbol}
    deriving (Eq, Ord, Show)

type Disjunct = [Liter]

type Conjunct = [Liter]

type DNF = [Conjunct]

type CNF = [Disjunct]

type UniversalQuant = String

data SSF = SSF {quants :: [UniversalQuant], matrix :: CNF} deriving (Eq, Ord, Show)
