{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module ConverterToSSF where

import           Ast
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Lazy
import           Data.List
import           Ssf

type Replacements = [(String, Term)]
type ForallVars = [String]

(.>) :: (a -> b) -> (b -> c) -> a -> c
f .> g = g . f

replace :: (a -> Bool) -> a -> [a] -> [a]
replace f x' [] = []
replace f x' (x : xs) =
    if f x then
        x' : xs
    else
        x : replace f x' xs

renameBoundVariables :: Formula -> Formula
renameBoundVariables = renamingInFormula .> flip evalState [] where
    renamingInFormula :: Formula -> State Replacements Formula
    renamingInFormula formula = case formula of
        Top -> return Top
        Bottom -> return Bottom
        PredicateSymbol (Symbol name args) -> do
            args' <- foldr (\arg args -> (:) <$> arg <*> args) (return []) $ renamingInTerm <$> args
            return $ PredicateSymbol $ Symbol name args'
        Neg f -> do
            f' <- renamingInFormula f
            return $ Neg f'
        Conj f1 f2 -> do
            f1' <- renamingInFormula f1
            f2' <- renamingInFormula f2
            return $ Conj f1' f2'
        Disj f1 f2 -> do
            f1' <- renamingInFormula f1
            f2' <- renamingInFormula f2
            return $ Disj f1' f2'
        Impl f1 f2 -> do
            f1' <- renamingInFormula f1
            f2' <- renamingInFormula f2
            return $ Impl f1' f2'
        Exist v f -> do
            replacements <- get
            case find (fst .> (== v)) replacements of
                Nothing -> do
                    let v' = "(" ++ v ++ ")"
                    modify ((v, Variable v') :)
                    f' <- renamingInFormula f
                    return $ Exist v' f'
                Just (_, Variable v') -> do
                    let v'' = v' ++ "'"
                    modify $ replace (fst .> (== v)) (v, Variable v'')
                    f' <- renamingInFormula f
                    return $ Exist v'' f'
                Just (_, FunctionSymbol fs) -> error "Replacement on Function Symbol is not meant"
        Forall v f -> do
            replacements <- get
            case find (fst .> (== v)) replacements of
                Nothing -> do
                    let v' = "(" ++ v ++ ")"
                    modify ((v, Variable v') :)
                    f' <- renamingInFormula f
                    return $ Forall v' f'
                Just (_, Variable v') -> do
                    let v'' = v' ++ "'"
                    modify $ replace (fst .> (== v)) (v, Variable v'')
                    f' <- renamingInFormula f
                    return $ Forall v'' f'
                Just (_, FunctionSymbol fs) -> error "Replacement on Function Symbol is not meant"
    renamingInTerm :: Term -> State Replacements Term
    renamingInTerm = \case
        Variable v -> do
            replacements <- get
            case find (fst .> (== v)) replacements of
                Nothing     -> return $ Variable v
                Just (_, t) -> return t
        FunctionSymbol (Symbol name args) -> do
            args' <- foldr (\arg args -> (:) <$> arg <*> args) (return []) $ renamingInTerm <$> args
            return $ FunctionSymbol $ Symbol name args'

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
                    Exist v f2'  -> Exist v (takeOutInConj f1 f2')
                    Forall v f2' -> Forall v (takeOutInConj f1 f2')
                    _            -> Conj f1 f2
    Disj f1 f2 -> takeOutInDisj (takeOutQuants f1) (takeOutQuants f2) where
        takeOutInDisj :: Formula -> Formula -> Formula
        takeOutInDisj f1 f2 = case f1 of
            Exist v f1' -> Exist v (takeOutInDisj f1' f2)
            Forall v f1' -> Forall v (takeOutInDisj f1' f2)
            _ ->
                case f2 of
                    Exist v f2'  -> Exist v (takeOutInDisj f1 f2')
                    Forall v f2' -> Forall v (takeOutInDisj f1 f2')
                    _            -> Disj f1 f2
    Impl f1 f2 -> takeOutInImpl (takeOutQuants f1) (takeOutQuants f2) where
        takeOutInImpl :: Formula -> Formula -> Formula
        takeOutInImpl f1 f2 = case f1 of
            Exist v f1' -> Forall v (takeOutInImpl f1' f2)
            Forall v f1' -> Exist v (takeOutInImpl f1' f2)
            _ ->
                case f2 of
                    Exist v f2'  -> Exist v (takeOutInImpl f1 f2')
                    Forall v f2' -> Forall v (takeOutInImpl f1 f2')
                    _            -> Impl f1 f2
    Exist v f -> Exist v (takeOutQuants f)
    Forall v f -> Forall v (takeOutQuants f)

deleteExistQuants :: Formula -> Formula
deleteExistQuants = renamingInFormula .> flip evalStateT [] .> flip evalState [] where
    renamingInFormula :: Formula -> StateT Replacements (State ForallVars) Formula
    renamingInFormula = \case
        Top -> return Top
        Bottom -> return Bottom
        PredicateSymbol (Symbol name args) -> do
            args' <- foldr (\arg args -> (:) <$> arg <*> args) (return []) $ renamingInTerm <$> args
            return $ PredicateSymbol (Symbol name args')
        Neg f -> do
            f' <- renamingInFormula f
            return $ Neg f'
        Conj f1 f2 -> do
            f1' <- renamingInFormula f1
            f2' <- renamingInFormula f2
            return $ Conj f1' f2'
        Disj f1 f2 -> do
            f1' <- renamingInFormula f1
            f2' <- renamingInFormula f2
            return $ Disj f1' f2'
        Impl f1 f2 -> do
            f1' <- renamingInFormula f1
            f2' <- renamingInFormula f2
            return $ Impl f1' f2'
        Exist v f -> do
            forallVars <- lift $ get
            let t = FunctionSymbol (Symbol ("Sko" ++ v) (Variable <$> forallVars))
            modify $ ((v, t) :)
            renamingInFormula f
        Forall v f -> do
            lift $ modify $ (v :)
            f' <- renamingInFormula f
            return $ Forall v f'
    renamingInTerm :: Term -> StateT Replacements (State ForallVars) Term
    renamingInTerm = \case
        Variable v -> do
            replacements <- get
            case find (fst .> (== v)) replacements of
                Nothing     -> return $ Variable v
                Just (_, t) -> return t
        FunctionSymbol (Symbol name args) -> do
            args' <- foldr (\arg args -> (:) <$> arg <*> args) (return []) $ renamingInTerm <$> args
            return $ FunctionSymbol (Symbol name args')

convertToCNF :: Formula -> CNF
convertToCNF = formula2CNF False .> simplify where
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
    _NF2_NF = simplify .> foldr (\x y -> concat $ (\z -> (: z) <$> x) <$> y) [[]] .> simplify
    simplify :: [[Liter]] -> [[Liter]]
    simplify [] = []
    simplify (x : xs) = let xs' = simplify xs in
        case simplify' x [] of
            [] -> xs'
            x' -> x' : xs'
    simplify' :: [Liter] -> [Liter] -> [Liter]
    simplify' [] ans = ans
    simplify' (x@(PS ps) : xs) ans =
        case find (getPS .> (== ps)) xs of
            Nothing        -> simplify' xs (x : ans)
            Just (PS _)    -> simplify' xs ans
            Just (NegPS _) -> []
    simplify' (x@(NegPS ps) : xs) ans =
        case find (getPS .> (== ps)) xs of
            Nothing        -> simplify' xs (x : ans)
            Just (NegPS _) -> simplify' xs ans
            Just (PS _)    -> []

convertToSSF :: Formula -> SSF
convertToSSF = renameBoundVariables .> takeOutQuants .> deleteExistQuants .> constructSSF where
    constructSSF :: Formula -> SSF
    constructSSF = \case
        Exist v f -> error "All existential quantifiers was deleted"
        Forall v f -> let (SSF quants matrix) = constructSSF f in
            SSF (v : quants) matrix
        f -> SSF [] (convertToCNF f)
