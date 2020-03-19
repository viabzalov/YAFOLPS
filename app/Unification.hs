{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Unification (unify) where

import           Ast
import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Maybe
import           Data.Set   (Set)
import qualified Data.Set   as Set
import           Ssf

type Substitution = Map String Term

type Equations = Set (Term, Term)

unify :: Equations -> Maybe Substitution
unify e = 
    case delete =<< (eliminate' 0) =<< check =<< swap =<< decompose e of
        Nothing -> Nothing
        Just e' ->
            case uncurry Set.disjoint $ (\(a, b) -> (Set.unions $ map variables a, Set.fromList b)) $ unzip $ Set.toList e' of
                False -> unify e'
                True -> Just $ Map.fromList $ map toSubstitution' $ Set.toList e' where
                    
                    toSubstitution' :: (Term, Term) -> (String, Term)
                    toSubstitution' (Variable name, t) = (name, t)
                    toSubstitution' (t1, t2) = error "This is not substitution!"


decompose :: Equations -> Maybe Equations
decompose e =
    if not $ Nothing `elem` e'
        then Just $ Set.fromList $ catMaybes e'
    else Nothing where
        e' = concat $ map decompose' $ Set.toList e

        decompose' :: (Term, Term) -> [Maybe (Term, Term)]
        decompose' (f1@(FunctionSymbol s1), f2@(FunctionSymbol s2)) =
            if (name s1 == name s2) && (length (args s1) == length (args s2))
                then [Just p | p <- zip (args s1) (args s2)]
            else [Nothing]
        decompose' x = [Just x]

swap :: Equations -> Maybe Equations
swap e = Just $ Set.map swap' e where

    swap' :: (Term, Term) -> (Term, Term)
    swap' (f@(FunctionSymbol s), v@(Variable name)) = (v, f)
    swap' p                                         = p

check :: Equations -> Maybe Equations
check e =
    if Set.member False (Set.map (\(v, t) -> not $ Set.member v (variables t)) e)
        then Nothing
    else Just e 

eliminate :: Int -> Equations -> Maybe Equations
eliminate i e = do
    let x = Set.elemAt i e
    case x of
        p@(v@(Variable name), t) ->
            if Set.member v vs
                then eliminate' 0 ne
            else
                if i + 1 >= Set.size e
                    then Just e
                else eliminate' (i + 1) e where
                    e' = Set.deleteAt i e
                    vs = variables e'
                    ne = Set.insert p (Set.map (\(k, v) -> (apply p k, apply p v)) e')
        otherwise ->
            if i + 1 >= Set.size e
                then Just e
            else eliminate' (i + 1) e

eliminate' :: Int -> Equations -> Maybe Equations
eliminate' i e =
    if (Set.size e == 0) || (i >= Set.size e)
        then Nothing
    else eliminate i e

delete :: Equations -> Maybe Equations
delete e = Just $ Set.filter (\(k, v) -> k /= v) e

class Variables a where
    variables :: a -> Set Term

instance Variables Term where
    variables (FunctionSymbol s) = Set.unions $ map variables (args s)
    variables v                  = Set.singleton v

instance Variables Equations where
    variables e = uncurry Set.union $ (\(a, b) -> (Set.unions $ map variables a, Set.unions $ map variables b)) $ unzip $ Set.toList e

apply :: (Term, Term) -> Term -> Term
apply p (FunctionSymbol s) =
    FunctionSymbol $ Symbol (name s) (map (apply p) (args s))
apply p v = if v == fst p then snd p else v
