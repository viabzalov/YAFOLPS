{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module ConverterToSSF where

import           Ast
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.List
import           Ssf

type Replacements = [(String, Term)]
type VarReplacements = [(String, String)]
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
renameBoundVariables = renamingInFormula .> flip runReaderT [] .> flip evalState 1 where
    renamingInFormula :: Formula -> ReaderT VarReplacements (State Int) Formula
    renamingInFormula formula = case formula of
        Top -> return Top
        Bottom -> return Bottom
        PredicateSymbol (Symbol name args) -> do
            args' <- foldM (\args m_arg -> m_arg >>= (\arg -> return $ arg : args)) [] $ renamingInTerm <$> args
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
            nextIndex <- lift $ get
            let v' = v ++ "_" ++ show nextIndex
            lift $ modify (+ 1)
            replacements <- ask
            f' <- case find (fst .> (== v)) replacements of
                Nothing ->
                    local ((v, v') :) $ renamingInFormula f
                Just _ ->
                    local (replace (fst .> (== v)) (v, v')) $ renamingInFormula f
            return $ Exist v' f'
        Forall v f -> do
            nextIndex <- lift $ get
            let v' = v ++ "_" ++ show nextIndex
            lift $ modify (+ 1)
            replacements <- ask
            f' <- case find (fst .> (== v)) replacements of
                Nothing ->
                    local ((v, v') :) $ renamingInFormula f
                Just _ ->
                    local (replace (fst .> (== v)) (v, v')) $ renamingInFormula f
            return $ Forall v' f'
    renamingInTerm :: Term -> ReaderT VarReplacements (State Int) Term
    renamingInTerm = \case
        Variable v -> do
            replacements <- ask
            case find (fst .> (== v)) replacements of
                Nothing      -> return $ Variable v
                Just (_, v') -> return $ Variable v'
        FunctionSymbol (Symbol name args) -> do
            args' <- foldM (\args m_arg -> m_arg >>= (\arg -> return $ arg : args)) [] $ renamingInTerm <$> args
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
deleteExistQuants = renamingInFormula .> flip runReaderT [] .> flip runReader [] where
    renamingInFormula :: Formula -> ReaderT Replacements (Reader ForallVars) Formula
    renamingInFormula = \case
        Top -> return Top
        Bottom -> return Bottom
        PredicateSymbol (Symbol name args) -> do
            args' <- foldM (\args m_arg -> m_arg >>= (\arg -> return $ arg : args)) [] $ renamingInTerm <$> args
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
            forallVars <- lift $ ask
            let t = FunctionSymbol (Symbol ("Sko(" ++ v ++ ")") (Variable <$> forallVars))
            local ((v, t) :) $ renamingInFormula f
        Forall v f -> do
            f' <- mapReaderT (local (v :)) $ renamingInFormula f
            return $ Forall v f'
    renamingInTerm :: Term -> ReaderT Replacements (Reader ForallVars) Term
    renamingInTerm = \case
        Variable v -> do
            replacements <- ask
            case find (fst .> (== v)) replacements of
                Nothing     -> return $ Variable v
                Just (_, t) -> return t
        FunctionSymbol (Symbol name args) -> do
            args' <- foldM (\args m_arg -> m_arg >>= (\arg -> return $ arg : args)) [] $ renamingInTerm <$> args
            return $ FunctionSymbol (Symbol name args')

convertToCNF :: Formula -> CNF
convertToCNF = formula2CNF .> simplify where
    formula2CNF :: Formula -> CNF
    formula2CNF = \case
        Top -> []
        Bottom -> [[]]
        PredicateSymbol ps -> [[PS ps]]
        Neg f -> formula2NegCNF f
        Conj f1 f2 -> formula2CNF f1 ++ formula2CNF f2
        Disj f1 f2 -> revealDistributively [formula2CNF f1, formula2CNF f2]
        Impl f1 f2 -> revealDistributively [formula2NegCNF f1, formula2CNF f2]
        Exist v f -> error "Quantifier cannot be converted to CNF"
        Forall v f -> error "Quantifier cannot be converted to CNF"
    formula2NegCNF :: Formula -> DNF
    formula2NegCNF = \case
        Top -> [[]]
        Bottom -> []
        PredicateSymbol ps -> [[NegPS ps]]
        Neg f -> formula2CNF f
        Conj f1 f2 -> revealDistributively [formula2NegCNF f1, formula2NegCNF f2]
        Disj f1 f2 -> formula2NegCNF f1 ++ formula2NegCNF f2
        Impl f1 f2 -> formula2CNF f1 ++ formula2NegCNF f2
        Exist v f -> error "Quantifier cannot be converted to CNF"
        Forall v f -> error "Quantifier cannot be converted to CNF"
    revealDistributively :: [[[Liter]]] -> [[Liter]]
    revealDistributively = map simplify .> sequence .> map concat .> simplify
    simplify :: [[Liter]] -> [[Liter]]
    simplify [] = []
    simplify ([] : xs) = [[]]
    simplify (x : xs) = let xs' = simplify xs in
        if xs' == [[]] then
            [[]]
        else
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
