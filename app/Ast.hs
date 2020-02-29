{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Ast where

-- Variable
type Var = Integer

-- FunctionSymbol
data FS = FS {name :: String, terms :: [Term]} deriving Show

-- PredicateSimbol
data PS = PS {name :: String, terms :: [Term]} deriving Show

-- Term
data Term =
      Variable Var
    | FunctionSymbol FS
    deriving Show

-- Formula
data Formula =
      Top
    | Bottom
    | PredicateSymbol PS
    | Neg Formula
    | Conj Formula Formula
    | Disj Formula Formula
    | Impl Formula Formula 
    | Exist Var Formula
    | Forall Var Formula
    deriving Show
