{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module ConverterToSSF where

import Ast
import Ssf

(.>) :: (a -> b) -> (b -> c) -> a -> c
f .> g = g . f

find :: (a -> Bool) -> [a] -> Maybe a
find f [] = Nothing
find f (x : xs) =
    if f x then
        Just x
    else
        find f xs

replace :: (a -> Bool) -> a -> [a] -> [a]
replace f x' [] = []
replace f x' (x : xs) =
    if f x then
        x' : xs
    else
        x : replace f x' xs

renameBoundVariables :: Formula -> Formula
renameBoundVariables = renamingInFormula [] where
    renamingInFormula :: [(String, String)] -> Formula -> Formula
    renamingInFormula changes = \case
        Top -> Top
        Bottom -> Bottom
        PredicateSymbol (Symbol name args) ->
            PredicateSymbol (Symbol name (renamingInTerm changes <$> args))
        Neg f ->
            Neg (renamingInFormula changes f)
        Conj f1 f2 ->
            Conj (renamingInFormula changes f1) (renamingInFormula changes f2)
        Disj f1 f2 ->
            Disj (renamingInFormula changes f1) (renamingInFormula changes f2)
        Impl f1 f2 ->
            Impl (renamingInFormula changes f1) (renamingInFormula changes f2)
        Exist v f ->
            case find (fst .> (== v)) changes of
                Nothing -> let v' = "(" ++ v ++ ")" in
                    Exist v' (renamingInFormula ((v, v') : changes) f)
                Just (_, v') -> let v'' = v' ++ "'" in
                    Exist v'' (renamingInFormula (replace (fst .> (== v)) (v, v'') changes) f)
        Forall v f ->
            case find (fst .> (== v)) changes of
                Nothing -> let v' = "(" ++ v ++ ")" in
                    Forall v' (renamingInFormula ((v, v') : changes) f)
                Just (_, v') -> let v'' = v' ++ "'" in
                    Forall v'' (renamingInFormula (replace (fst .> (== v)) (v, v'') changes) f)
    renamingInTerm :: [(String, String)] -> Term -> Term
    renamingInTerm changes = \case
        Variable v ->
            case find (fst .> (== v)) changes of
                Nothing -> Variable v
                Just (_, v') -> Variable v'
        FunctionSymbol (Symbol name args) ->
            FunctionSymbol (Symbol name (renamingInTerm changes <$> args))

takeOutQuants :: Formula -> Formula
takeOutQuants = \case
    Top -> Top
    Bottom -> Bottom
    PredicateSymbol ps -> PredicateSymbol ps
    Neg f -> takeOutInNeg (takeOutQuants f) where
        takeOutInNeg :: Formula -> Formula
        takeOutInNeg = \case
            Exist v f -> Forall v (takeOutInNeg f)
            Forall v f -> Exist v (takeOutInNeg f)
            f -> Neg f
    Conj f1 f2 -> takeOutInConj (takeOutQuants f1) (takeOutQuants f2) where
        takeOutInConj :: Formula -> Formula -> Formula
        takeOutInConj f1 f2 = case f1 of
            Exist v f1' -> Exist v (takeOutInConj f1' f2)
            Forall v f1' -> Forall v (takeOutInConj f1' f2)
            _ ->
                case f2 of
                    Exist v f2' -> Exist v (takeOutInConj f1 f2')
                    Forall v f2' -> Forall v (takeOutInConj f1 f2')
                    _ -> Conj f1 f2
    Disj f1 f2 -> takeOutInDisj (takeOutQuants f1) (takeOutQuants f2) where
        takeOutInDisj :: Formula -> Formula -> Formula
        takeOutInDisj f1 f2 = case f1 of
            Exist v f1' -> Exist v (takeOutInDisj f1' f2)
            Forall v f1' -> Forall v (takeOutInDisj f1' f2)
            _ ->
                case f2 of
                    Exist v f2' -> Exist v (takeOutInDisj f1 f2')
                    Forall v f2' -> Forall v (takeOutInDisj f1 f2')
                    _ -> Disj f1 f2
    Impl f1 f2 -> takeOutInImpl (takeOutQuants f1) (takeOutQuants f2) where
        takeOutInImpl :: Formula -> Formula -> Formula
        takeOutInImpl f1 f2 = case f1 of
            Exist v f1' -> Forall v (takeOutInImpl f1' f2)
            Forall v f1' -> Exist v (takeOutInImpl f1' f2)
            _ ->
                case f2 of
                    Exist v f2' -> Exist v (takeOutInImpl f1 f2')
                    Forall v f2' -> Forall v (takeOutInImpl f1 f2')
                    _ -> Impl f1 f2
    Exist v f -> Exist v (takeOutQuants f)
    Forall v f -> Forall v (takeOutQuants f)

deleteExistQuants :: Formula -> Formula
deleteExistQuants = renamingVarsInFormula [] [] where
    renamingVarsInFormula :: [String] -> [(String, Term)] -> Formula -> Formula
    renamingVarsInFormula forallVars changes = \case
        Top -> Top
        Bottom -> Bottom
        PredicateSymbol (Symbol name args) ->
            PredicateSymbol (Symbol name (renamingVarsInTerm changes <$> args))
        Neg f ->
            Neg (renamingVarsInFormula forallVars changes f)
        Conj f1 f2 ->
            Conj (renamingVarsInFormula forallVars changes f1)
                 (renamingVarsInFormula forallVars changes f2)
        Disj f1 f2 ->
            Disj (renamingVarsInFormula forallVars changes f1)
                 (renamingVarsInFormula forallVars changes f2)
        Impl f1 f2 ->
            Impl (renamingVarsInFormula forallVars changes f1)
                 (renamingVarsInFormula forallVars changes f2)
        Exist v f -> let t = FunctionSymbol (Symbol ("Sko" ++ v) (Variable <$> forallVars)) in
            renamingVarsInFormula forallVars ((v, t) : changes) f
        Forall v f ->
            Forall v (renamingVarsInFormula (forallVars ++ [v]) changes f)
    renamingVarsInTerm :: [(String, Term)] -> Term -> Term
    renamingVarsInTerm changes = \case
        Variable v ->
            case find (fst .> (== v)) changes of
                Nothing -> Variable v
                Just (_, t) -> t
        FunctionSymbol (Symbol name args) ->
            FunctionSymbol (Symbol name (renamingVarsInTerm changes <$> args))

convertToCNF :: Formula -> CNF
convertToCNF = formula2CNF False .> simplifyCNF where
    formula2CNF :: Bool -> Formula -> CNF
    formula2CNF isNeg = \case
        Top -> if isNeg then [[]] else []
        Bottom -> if isNeg then [] else [[]]
        PredicateSymbol ps -> [[if isNeg then NegPS ps else PS ps]]
        Neg f -> formula2DNF (not isNeg) f
        Conj f1 f2 -> formula2CNF isNeg f1 ++ formula2CNF isNeg f2
        Disj f1 f2 -> _NF2_NF $ formula2DNF isNeg f1 ++ formula2DNF isNeg f2
        Impl f1 f2 -> _NF2_NF $ formula2CNF (not isNeg) f1 ++ formula2DNF isNeg f2
        Exist v f -> error "Quantifier cannot be converted to CNF"
        Forall v f -> error "Quantifier cannot be converted to CNF"
    formula2DNF :: Bool -> Formula -> DNF
    formula2DNF isNeg = \case
        Top -> if isNeg then [] else [[]]
        Bottom -> if isNeg then [[]] else []
        PredicateSymbol ps -> [[if isNeg then NegPS ps else PS ps]]
        Neg f -> formula2CNF (not isNeg) f
        Conj f1 f2 -> _NF2_NF $ formula2CNF isNeg f1 ++ formula2CNF isNeg f2
        Disj f1 f2 -> formula2DNF isNeg f1 ++ formula2DNF isNeg f2
        Impl f1 f2 -> formula2CNF (not isNeg) f1 ++ formula2DNF isNeg f2
        Exist v f -> error "Quantifier cannot be converted to CNF"
        Forall v f -> error "Quantifier cannot be converted to CNF"
    _NF2_NF :: [[Liter]] -> [[Liter]]
    _NF2_NF [] = [[]]
    _NF2_NF (x : xs) = concat $ (\y -> (: y) <$> x) <$> _NF2_NF xs
    simplifyCNF :: CNF -> CNF
    simplifyCNF [] = []
    simplifyCNF (x : xs) = let xs' = simplifyCNF xs in
        case simplifyDisjunct x [] of
            [] -> xs'
            x' -> x' : xs'
    simplifyDisjunct :: Disjunct -> Disjunct -> Disjunct
    simplifyDisjunct [] ans = ans
    simplifyDisjunct (x@(PS ps) : xs) ans =
        case find (getPS .> (== ps)) xs of
            Nothing -> simplifyDisjunct xs (x : ans)
            Just (PS _) -> simplifyDisjunct xs ans
            Just (NegPS _) -> []
    simplifyDisjunct (x@(NegPS ps) : xs) ans =
        case find (getPS .> (== ps)) xs of
            Nothing -> simplifyDisjunct xs (x : ans)
            Just (NegPS _) -> simplifyDisjunct xs ans
            Just (PS _) -> []

convertToSSF :: Formula -> SSF
convertToSSF = renameBoundVariables .> takeOutQuants .> deleteExistQuants .> constructSSF where
    constructSSF :: Formula -> SSF
    constructSSF = \case
        Exist v f -> error "All existential quantifiers was deleted"
        Forall v f -> let (SSF quants matrix) = constructSSF f in
            SSF (v : quants) matrix
        f -> SSF [] (convertToCNF f)
