module Ast where

-- Variable
type Var = Integer

-- FunctionSymbol
data FS = FS {name :: String, terms :: [Term]}

-- PredicateSimbol
data PS = PS {name :: String, terms :: [Term]}

-- Term
data Term =
      Variable Var
    | FunctionSymbol FS

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
