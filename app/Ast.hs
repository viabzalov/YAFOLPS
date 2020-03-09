{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Ast where

-- FunctionSymbol or PredicateSymbol
data Symbol = Symbol {name :: String, args :: [Term]} deriving Show

instance Eq Symbol where
    (==) (Symbol name_1 args_1) (Symbol name_2 args_2) =
        name_1 == name_2 && args_1 == args_2

-- Term
data Term =
      Variable String
    | FunctionSymbol Symbol
    deriving Show

instance Eq Term where
    (==) (Variable name_1) (Variable name_2) =
        name_1 == name_2
    (==) (FunctionSymbol symbol_1) (FunctionSymbol symbol_2) =
        symbol_1 == symbol_2
    (==) _ _ = False

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
    deriving Show
