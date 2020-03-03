{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Ast where

-- Variable
type Var = String

-- FunctionSymbol or PredicateSymbol
data Symbol = Symbol {name :: String, args :: [Term]}

instance Show Symbol where
    show (Symbol name args) = show name ++ show args

-- Term
data Term =
      Variable Var
    | FunctionSymbol Symbol
    deriving Show

-- Formula
data Formula =
      Top
    | Bottom
    | PredicateSymbol Symbol
    | Neg Formula
    | Conj Formula Formula
    | Disj Formula Formula
    | Impl Formula Formula 
    | Exist Var Formula
    | Forall Var Formula
    deriving Show
