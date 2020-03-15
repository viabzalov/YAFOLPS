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
type Equations = Map Term Term

unify :: Equations -> [Substitution]
unify e = do
    e' <- maybeToList (delete =<< (eliminate' 0) =<< check =<< swap =<< decompose e)
    if isSubstitution e'
        then [Map.mapKeys (\(Variable name) -> name) e']
    else unify e'

decompose :: Equations -> Maybe Equations
decompose e =
    if not $ Nothing `elem` e'
        then Just $ Map.fromList $ catMaybes e'
    else Nothing where
        e' = concat $ map decompose' $ Map.toList e

        decompose' :: (Term, Term) -> [Maybe (Term, Term)]
        decompose' (f1@(FunctionSymbol s1), f2@(FunctionSymbol s2)) =
            if (name s1 == name s2) && (length (args s1) == length (args s2))
                then [Just p | p <- zip (args s1) (args s2)]
            else [Nothing]
        decompose' x = [Just x]

swap :: Equations -> Maybe Equations
swap e = Just $ Map.fromList $ map swap' (Map.toList e) where

    swap' :: (Term, Term) -> (Term, Term)
    swap' (f@(FunctionSymbol s), v@(Variable name)) = (v, f)
    swap' p                                         = p

check :: Equations -> Maybe Equations
check s =
    if False `elem` (Map.elems $ Map.mapWithKey check' s)
        then Nothing
    else Just s where

        check' :: Term -> Term -> Bool
        check' v t = not $ Set.member v (variables t)

eliminate :: Int -> Equations -> Maybe Equations
eliminate i s = do
    let x = Map.elemAt i s
    case x of
        (v@(Variable name), trm) ->
            if Set.member v vs
                then eliminate' 0 ns
            else
                if i + 1 >= Map.size s
                    then Just s
                else eliminate' (i + 1) s where
                    s' = Map.deleteAt i s
                    vs = variables s'
                    sng = (Map.singleton v trm)
                    ns = Map.insert v trm (apply sng s')
        otherwise ->
            if i + 1 >= Map.size s
                then Just s
            else eliminate' (i + 1) s

eliminate' :: Int -> Equations -> Maybe Equations
eliminate' i s =
    if (Map.size s == 0) || (i >= Map.size s)
        then Nothing
    else eliminate i s

delete :: Equations -> Maybe Equations
delete s = Just $ Map.filterWithKey (\k v -> k /= v) s

isSubstitution :: Equations -> Bool
isSubstitution s = Set.disjoint ks vs where
    ks = Map.keysSet s
    vs = Set.unions $ map variables (Map.elems s)

class Variables a where
    variables :: a -> Set Term
    apply :: Equations -> a -> a

instance Variables Term where
    variables (FunctionSymbol s) = Set.unions $ map variables (args s)
    variables v                  = Set.singleton v

    apply p (FunctionSymbol s) =
        FunctionSymbol $ Symbol (name s) (map (apply p) (args s))
    apply p v = Map.findWithDefault v v p

instance Variables Equations where
    variables e = Set.union ks vs where
        ks = Map.keysSet e
        vs = Set.unions $ map variables (Map.elems e)

    apply p s = Map.map (apply p) (Map.mapKeys (apply p) s)
