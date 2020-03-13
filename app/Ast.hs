{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Ast where

-- FunctionSymbol or PredicateSymbol
data Symbol = Symbol {name :: String, args :: [Term]} deriving (Eq, Ord, Show)

-- Term
data Term =
      Variable String
    | FunctionSymbol Symbol
    deriving (Eq, Ord, Show)

-- Formula
data Formula =
      Top
    | Bottom
    | PredicateSymbol Symbol
    | Neg Formula
    | Conj Formula Formula
    | Disj Formula Formula
    | Impl Formula Formula
    | Exist String Formula
    | Forall String Formula
    deriving (Eq, Ord, Show)
