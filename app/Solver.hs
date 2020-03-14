{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Solver where

import           Ast
import           Ssf

import           Control.Monad
import           Data.Foldable
import           Data.List
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Maybe
import           Data.Set            (Set)
import qualified Data.Set            as Set
import qualified Data.Tuple          as T

import           Debug.Pretty.Simple
import Debug.Trace

type Substitution = Map Term Term

type MyDisjunct = Set Liter
type MyCNF = Set MyDisjunct

solve :: SSF -> Bool
solve (SSF quants []) = True
solve (SSF quants cnf) =
    if find (== []) cnf /= Nothing
        then False
    else 
        and $ solveCNF 100 (uniqCNF 1 (Set.fromList $ map (Set.fromList) cnf))

solveCNF :: Int -> MyCNF -> [Bool]
solveCNF 0 _ = [True]
solveCNF n cnf = do
    i <- [0..(Set.size cnf - 1)]
    j <- [(i + 1)..(Set.size cnf - 1)]
    let d1 = Set.elemAt i cnf
    let d2 = Set.elemAt j cnf
    let g1 = gluing d1
    let g2 = gluing d2
    d1' <- if g1 == [] then [Set.empty] else g1
    d2' <- if g2 == [] then [Set.empty] else g2
    let r1 = Set.fromList $ resolution d1' d2'
    let r2 = Set.fromList $ resolution d1' d2
    let r3 = Set.fromList $ resolution d1 d2'
    let r4 = Set.fromList $ resolution d1 d2
    let r = uniqCNF 1 (Set.unions [cnf, r1, r2, r3, r4])
    if Set.member Set.empty r
        then [False]
    else solveCNF (n - 1) (uniqCNF 1 r)


class Logic a where
    apply :: Substitution -> a -> a
    equiv :: a -> a -> Bool
    variables :: a -> Set Term
    uniq' :: Int -> a -> a
    uniq :: a -> a
    unify :: a -> a -> [Substitution]

instance Logic Substitution where

    apply p s = Map.map (apply p) (Map.mapKeys (apply p) s)

    equiv s1 s2 = s1 == s2

    variables s = Set.union ks vs where
        ks = Map.keysSet s
        vs = Set.unions $ map variables (Map.elems s)

    uniq' n s = apply p s where
        ks = Set.toList $ variables s
        vs = map (Variable . show) [n .. (n + length ks)]
        p = Map.fromList $ zip ks vs

    uniq s = uniq' 1 s

    unify s1 s2 = unify' $ Map.union s1 s2 where

        unify' :: Substitution -> [Substitution]
        unify' s = do
            s' <- maybeToList (unique =<< (eliminate' 0) =<< occursCheck =<< swap =<< (decompose' 0 s))
            if isSubstitution s' 
                then [s'] 
            else unify' s'

        unique :: Substitution -> Maybe Substitution
        unique s = Just $ Map.filterWithKey (\k v -> k /= v) s

        isSubstitution :: Substitution -> Bool
        isSubstitution s = Set.disjoint ks vs where
            ks = Map.keysSet s
            vs = Set.unions $ map variables (Map.elems s)

        decompose :: Int -> Substitution -> Maybe Substitution
        decompose i s = do
            let x = Map.elemAt i s
            case x of
                (f1@(FunctionSymbol s1), f2@(FunctionSymbol s2)) ->
                    if equiv f1 f2
                        then decompose' 0 s'
                    else Nothing where
                        s1' = Map.fromList (zip (args s1) (args s2))
                        s2' = Map.deleteAt i s
                        s' = Map.union s1' s2'
                otherwise ->
                    if i + 1 >= Map.size s
                        then Just s
                    else decompose' (i + 1) s
        
        decompose' :: Int -> Substitution -> Maybe Substitution
        decompose' i s =
            if (Map.size s == 0) || (i >= Map.size s)
                then Nothing
            else decompose i s
        
        swap :: Substitution -> Maybe Substitution
        swap s = Just s' where
            (t, f) = Map.partitionWithKey swap' s
            t' = Map.fromList $ map T.swap (Map.toList t)
            s' = Map.union t' f

        swap' :: Term -> Term -> Bool
        swap' t1 t2 = 
            case (t1, t2) of
                (f@(FunctionSymbol s), v@(Variable name)) -> True
                otherwise -> False
        
        eliminate :: Int -> Substitution -> Maybe Substitution
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
                            ns = Map.union sng (apply sng s')
                otherwise -> 
                    if i + 1 >= Map.size s
                        then Just s
                    else eliminate' (i + 1) s

        eliminate' :: Int -> Substitution -> Maybe Substitution
        eliminate' i s = 
            if (Map.size s == 0) || (i >= Map.size s)
                then Nothing
            else eliminate i s

        occursCheck :: Substitution -> Maybe Substitution
        occursCheck s = 
            if and $ map (check s) [0..(Map.size s - 1)]
                then Just s
            else Nothing

        check :: Substitution -> Int -> Bool
        check s i = res where
            (v, t) = Map.elemAt i s
            res = not $ Set.member v (variables t)

instance Logic Term where
 
    apply p (FunctionSymbol s) = 
        FunctionSymbol $ Symbol (name s) (map (apply p) (args s))
    apply p v = Map.findWithDefault v v p

    equiv v@(Variable n) f@(FunctionSymbol s) = 
        not $ Set.member v (variables f)
    equiv (FunctionSymbol s1) (FunctionSymbol s2) =
        (name s1) == (name s2) &&
        (length (args s1)) == (length (args s2)) &&
        and (map (uncurry equiv) (zip (args s1) (args s2)))
    equiv f@(FunctionSymbol s) v@(Variable n) = equiv v f
    equiv _ _ = True

    variables (FunctionSymbol s) = Set.unions $ map variables (args s)
    variables v = Set.singleton v

    uniq' n t = apply p t where
        ks = Set.toList $ variables t
        vs = map (Variable . show) [n..(n + length ks)]
        p = Map.fromList $ zip ks vs

    uniq t = uniq' 1 t

    unify t1 t2 = 
        if t1 == t2
            then [Map.empty]
        else 
            unify (Map.fromList [(t1, t2)]) (Map.empty)

instance Logic Liter where

    apply p (PS s) = PS $ Symbol (name s) (map (apply p) (args s))
    apply p (NegPS s) = NegPS $ Symbol (name s) (map (apply p) (args s))

    equiv l1 l2 = equiv (FunctionSymbol $ getPS l1) (FunctionSymbol $ getPS l2)

    variables l = variables (FunctionSymbol $ getPS l)

    uniq' n l = apply p l where
        ks = Set.toList $ variables l
        vs = map (Variable . show) [n .. (n + length ks)]
        p = Map.fromList $ zip ks vs

    uniq l = uniq' 1 l

    unify l1 l2 = 
        if getPS l1 == getPS l2 
            then [Map.empty]
        else 
            unify (FunctionSymbol $ getPS l1) (FunctionSymbol $ getPS l2)

instance Logic MyDisjunct where

    apply p d = Set.map (apply p) d

    equiv d1 d2 = 
        and $ map (uncurry equiv) (zip (Set.toList d1) (Set.toList d2))

    variables d =
        Set.unions $ map variables (Set.toList d)

    uniq' n d = apply p d where
        ks = Set.toList $ variables d
        vs = map (Variable . show) [n .. (n + length ks)]
        p = Map.fromList $ zip ks vs

    uniq d = uniq' 1 d

    unify d1 d2 = do
        i <- [0..(Set.size d1 - 1)]
        j <- [0..(Set.size d2 - 1)]
        let l1 = Set.elemAt i d1
        let l2 = Set.elemAt j d2
        if equiv' l1 l2 
            then unify l1 l2
        else [] where
            equiv' :: Liter -> Liter -> Bool
            equiv' (PS s1) (PS s2) = True
            equiv' (NegPS s1) (NegPS s2) = True
            equiv' _ _ = False

resolution :: MyDisjunct -> MyDisjunct -> [MyDisjunct]
resolution d1 d2 = do
    i <- [0..(Set.size d1 - 1)]
    j <- [0..(Set.size d2 - 1)]
    let l1 = Set.elemAt i d1
    let l2 = Set.elemAt j d2
    p <- unify l1 l2
    if (equiv' l1 l2)
        then [apply p (Set.union (Set.deleteAt i d1) (Set.deleteAt j d2))]
    else [] where
        equiv' :: Liter -> Liter -> Bool
        equiv' (NegPS s1) (PS s2) = True
        equiv' (PS s1) (NegPS s2) = True
        equiv' _ _ = False

gluing :: MyDisjunct -> [MyDisjunct]
gluing d = do
    i <- [0..(Set.size d - 1)]
    j <- [(i + 1)..(Set.size d - 1)]
    let l1 = Set.elemAt i d
    let l2 = Set.elemAt j d
    p <- unify l1 l2
    if equiv' l1 l2 
        then [apply p (Set.deleteAt i d)]
    else [] where
        equiv' :: Liter -> Liter -> Bool
        equiv' (PS s1) (PS s2) = True
        equiv' (NegPS s1) (NegPS s2) = True
        equiv' _ _ = False

uniqCNF :: Int -> MyCNF -> MyCNF
uniqCNF n cnf = 
    if Set.size cnf == 0 
        then Set.empty
    else
        Set.union (uniqCNF (n + len) s1) (Set.singleton $ uniq' n s0) where
            s0 = Set.elemAt 0 cnf
            s1 =  Set.deleteAt 0 cnf
            len = Set.size $ variables s0
    
