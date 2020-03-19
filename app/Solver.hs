{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Solver where

import           Ast
import           Ssf
import           Unification

import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.Maybe
import           Data.Set    (Set)
import qualified Data.Set    as Set

type Substitution = Map String Term

type MyDisjunct = Set Literal
type MyCNF = Set MyDisjunct

solve :: SSF -> Bool
solve (SSF quants []) = True
solve (SSF quants cnf) =
    if [] `elem` cnf
        then False
    else
        and $ solveCNF 4 (renameCNF (1, (Set.fromList $ map (Set.fromList) cnf)))

solveCNF :: Int -> (Int, MyCNF) -> [Bool]
solveCNF 0 _ = [True]
solveCNF h (n, cnf) = do
    i <- [0..(Set.size cnf - 1)]
    j <- [(i + 1)..(Set.size cnf - 1)]
    let d1 = Set.elemAt i cnf
    let d2 = Set.elemAt j cnf
    let (n1, rd1) = rename (n, d1)
    let (n2, rd2) = rename (n1, d2)
    let g1 = gluing rd1
    let g2 = gluing rd2
    d1' <- if g1 == [] then [Set.empty] else g1
    d2' <- if g2 == [] then [Set.empty] else g2
    let r1 = Set.fromList $ resolution d1' d2'
    let r2 = Set.fromList $ resolution d1' d2
    let r3 = Set.fromList $ resolution d1 d2'
    let r4 = Set.fromList $ resolution d1 d2
    let r = Set.unions [cnf, r1, r2, r3, r4]
    if Set.member Set.empty r
        then [False]
    else solveCNF (h - 1) (n2, r)

resolution :: MyDisjunct -> MyDisjunct -> [MyDisjunct]
resolution d1 d2 = do
    i <- [0..(Set.size d1 - 1)]
    j <- [0..(Set.size d2 - 1)]
    let l1 = Set.elemAt i d1
    let l2 = Set.elemAt j d2
    let p = fromMaybe Map.empty $ unify $ Set.fromList $ zip ((args . getPS) l1) ((args . getPS) l2)
    if equiv' p l1 l2
        then [apply p (Set.union (Set.deleteAt i d1) (Set.deleteAt j d2))]
    else [] where
        equiv' :: Substitution -> Literal -> Literal -> Bool
        equiv' p (NegPS s1) (PS s2) = (apply p $ FunctionSymbol s1) == (apply p $ FunctionSymbol s2)
        equiv' p (PS s1) (NegPS s2) = (apply p $ FunctionSymbol s1) == (apply p $ FunctionSymbol s2)
        equiv' _ _ _                      = False

gluing :: MyDisjunct -> [MyDisjunct]
gluing d = do
    i <- [0..(Set.size d - 1)]
    j <- [(i + 1)..(Set.size d - 1)]
    let l1 = Set.elemAt i d
    let l2 = Set.elemAt j d
    let p = fromMaybe Map.empty $ unify $ Set.fromList $ zip ((args . getPS) l1) ((args . getPS) l2)
    if equiv' p l1 l2
        then [apply p (Set.deleteAt i d)]
    else [] where
        equiv' :: Substitution -> Literal -> Literal -> Bool
        equiv' p l1@(PS s1) l2@(PS s2)       = (apply p l1) == (apply p l2)
        equiv' p l1@(NegPS s1) l2@(NegPS s2) = (apply p l1) == (apply p l2)
        equiv' _ _ _                         = False

class Apply a where
    apply :: Substitution -> a -> a

instance Apply Term where
    apply p (FunctionSymbol s) = FunctionSymbol $ Symbol (name s) (map (apply p) (args s))
    apply p v@(Variable name) = Map.findWithDefault v name p

instance Apply Literal where
    apply p (PS l)    = PS $ Symbol (name l) (map (apply p) (args l))
    apply p (NegPS l) = NegPS $ Symbol (name l) (map (apply p) (args l))

instance Apply MyDisjunct where
    apply p d = Set.map (apply p) d

variables :: MyDisjunct -> Set String
variables d = Set.unions $ Set.map (Set.unions . (map variablesOfTerm) . args . getPS) d where

    variablesOfTerm :: Term -> Set String
    variablesOfTerm (FunctionSymbol s) = Set.unions $ map variablesOfTerm (args s)
    variablesOfTerm (Variable name)    = Set.singleton name

rename :: (Int, MyDisjunct) -> (Int, MyDisjunct)
rename (n, d) = (n', apply p d) where
    vs = variables d
    n' = n + Set.size vs
    p = Map.fromList $ zip (Set.toList vs) (map (Variable . show) [n .. (n' - 1)])

renameCNF :: (Int, MyCNF) -> (Int, MyCNF)
renameCNF (n, cnf) =
    if Set.size cnf == 0
        then (n, cnf)
    else (n', Set.insert d0 cnf') where
        (n0, d0) = rename (n, Set.elemAt 0 cnf)
        (n', cnf') = renameCNF (n0, Set.deleteAt 0 cnf)
