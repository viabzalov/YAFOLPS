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
import           Data.Maybe
import qualified Data.Tuple    as T

type Substitution = [(Term, Term)]

firstJust :: [Maybe a] -> Maybe a
firstJust = join . find isJust

composition :: Substitution -> Substitution -> Substitution
composition sbstn1 sbstn2 = unique $ sbstn1' ++ sbstn2' where
    unz_sbstn1 = unzip sbstn1
    unz_sbstn2 = unzip sbstn2
    tmp = zip (fst unz_sbstn1) (snd unz_sbstn2)
    sbstn1' = zip (fst unz_sbstn1) (map (apply [sbstn2]) (snd unz_sbstn1))
    sbstn2' = [x | x <- sbstn2, not $ x `elem` tmp]

unique :: Substitution -> Substitution
unique sbstn = [x | x@(fi, se) <- sbstn, fi /= se]

decompose :: Substitution -> Maybe Substitution
decompose [] = Just $ []
decompose (pr:prs) = case pr of
    (f1@(FunctionSymbol s1), f2@(FunctionSymbol s2)) ->
        if (equiv f1 f2) then (decompose $ (zip (args s1) (args s2)) ++ prs)
        else Nothing
    otherwise -> case decompose prs of
        Just prs' -> Just $ pr:prs'
        Nothing   -> Nothing

swap :: Substitution -> Substitution
swap [] = []
swap (pr:prs) = case pr of
    (f@(FunctionSymbol s), v@(Variable name)) -> (v, f):(swap prs)
    otherwise                                 -> pr:(swap prs)

eliminate :: Substitution -> Substitution -> Substitution
eliminate [] sbstn = sbstn
eliminate (pr:prs) sbstn = case pr of
    (v@(Variable name), trm) ->
        if (occursCheck [pr] /= Nothing) && (v `elem` vrbs)
            then eliminate sbstn' sbstn'
        else eliminate prs sbstn where
            sbstn'' = delete pr sbstn
            (ls, rs) = unzip sbstn''
            vrbs = foldl (union) [] ((map getVariables ls) ++ (map getVariables rs))
            replace' :: (Term, Term) -> (Term, Term) -> (Term, Term)
            replace' pr (fi, se) = ((apply [[pr]] fi), (apply [[pr]] se))
            sbstn' = pr:(map (replace' pr) sbstn'')
    otherwise -> eliminate prs sbstn

occursCheck :: Substitution -> Maybe Substitution
occursCheck sbstn =
    if and $ map check sbstn then Just $ sbstn else Nothing where
        check :: (Term, Term) -> Bool
        check (vrb, trm) = not $ vrb `elem` (getVariables trm)

unify' :: Substitution -> Maybe Substitution
unify' sbstn = case decompose sbstn of
    Nothing -> Nothing
    Just sbstn1 -> case occursCheck (swap sbstn1) of
        Nothing -> Nothing
        Just sbstn2 ->
            if isSubstitution sbstn3 then Just $ sbstn3
            else unify' sbstn3 where
                sbstn3 = unique $ swap $ eliminate sbstn2 sbstn2

isSubstitution :: Substitution -> Bool
isSubstitution sbstn = ans where

    isVariables :: [Term] -> Bool
    isVariables [] = True
    isVariables (trm:trms) = case trm of
        (Variable name) -> True && (isVariables trms)
        otherwise       -> False

    (ls, rs) = unzip sbstn
    rVariables = foldl (union) [] (map getVariables rs)

    ans = (isVariables ls) &&
          (and $ [not (x `elem` rVariables) | x <- ls])

class Unifiable a where
    getVariables :: a -> [Term]
    apply :: [Substitution] -> a -> a
    equiv :: a -> a -> Bool
    unify :: a -> a -> Maybe ((a, a), Substitution)
    uniq :: Int -> a -> a

instance Unifiable Term where
    getVariables (FunctionSymbol smbl) = nub $ concatMap getVariables (args smbl)
    getVariables vrb                   = [vrb]

    apply [] trm = trm
    apply [sbstn] vrb@(Variable name)  = case lookup vrb sbstn of
        Just trm  -> trm
        otherwise -> vrb
    apply [sbstn] (FunctionSymbol smbl) =
        FunctionSymbol $ Symbol (name smbl) (map (apply [sbstn]) (args smbl))
    apply (sbstn1:sbstn2:sbstns) trm = apply ((composition sbstn1 sbstn2):sbstns) trm

    equiv (Variable name1) (Variable name2) = (name1 == name2)
    equiv (FunctionSymbol smbl1) (FunctionSymbol smbl2) =
        (name smbl1) == (name smbl2) &&
        (length (args smbl1)) == (length (args smbl2))
    equiv _ _ = False

    unify trm1 trm2 = case unify' [(trm1, trm2)] of
        Nothing    -> Nothing
        Just sbstn -> Just $ ((trm1, trm2), sbstn)

    uniq n trm = trm' where
        vrbs = getVariables trm
        vrbs' = map (Variable . show) [n..(n + length vrbs)]
        trm' = apply [zip vrbs vrbs'] trm
        

instance Unifiable Liter where
    getVariables ltr = getVariables $ FunctionSymbol $ getPS $ ltr

    apply sbstns (PS smbl) = PS $ Symbol (name smbl) (map (apply sbstns) (args smbl))
    apply sbstns (NegPS smbl) = NegPS $ Symbol (name smbl) (map (apply sbstns) (args smbl))

    equiv ltr1 ltr2 = equiv (FunctionSymbol $ getPS $ ltr1) (FunctionSymbol $ getPS $ ltr2)

    unify ltr1 ltr2 = case unify (FunctionSymbol $ getPS $ ltr1) (FunctionSymbol $ getPS $ ltr2) of
        Nothing         -> Nothing
        Just (x, sbstn) -> Just $ ((ltr1, ltr2), sbstn)

    uniq n ltr = ltr' where
        vrbs = getVariables ltr
        vrbs' = map (Variable . show) [n..(n + length vrbs)]
        ltr' = apply [zip vrbs vrbs'] ltr

instance Unifiable Disjunct where
    getVariables disj = nub $ concatMap getVariables disj

    apply sbstns disj = map (apply sbstns) disj

    equiv disj1 disj2 = and $ map (uncurry equiv) (zip disj1 disj2)

    unify disj1 disj2 =
        if sbstn' == Nothing then Nothing
        else Just $ ((r1, r2), sbstn) where
            r1 = nub $ delete ltr1 $ delete ltr2 $ apply [sbstn] disj1
            r2 = nub $ delete ltr1 $ delete ltr2 $ apply [sbstn] disj2

            ltr1 = apply [sbstn] ((fst . fst) pr)
            ltr2 = apply [sbstn] ((snd . fst) pr)

            l1 = [unify x y | x@(PS s1) <- disj1, y@(NegPS s2) <- disj2]
            l2 = [unify x y | x@(NegPS s1) <- disj1, y@(PS s2) <- disj2]

            sbstn' = firstJust $ l1 ++ l2
            sbstn = snd pr
            pr = case sbstn' of
                Nothing -> ((ltr1, ltr2), sbstn)
                Just x -> x

    uniq n disj = disj' where
        vrbs = getVariables disj
        vrbs' = map (Variable . show) [n..(n + length vrbs)]
        disj' = apply [zip vrbs vrbs'] disj

solveCNF :: CNF -> Bool
solveCNF [] = True
solveCNF cnf =
    if (find (== []) cnf) /= Nothing then False
    else solveCNF cnf' where
        cnf' = concat $
            [
                case unify x y of
                    Nothing         -> []
                    Just ((d1, d2), sbstn) -> [nub $ d1 ++ d2]

            | x <- newCNF, y <- newCNF, x /= y]

        newCNF = cnf ++ cnf''

        cnf'' = concat $ 
            [
                case unify y z of
                    Nothing         -> []
                    Just ((l1, l2), sbstn) -> [nub $ apply [sbstn] x]

            | x <- cnf, y <- x, z <- x, y/= z]

uniq'' :: Int -> CNF -> CNF
uniq'' n disjs =
    if length disjs == 1 then [uniq n (head disjs)]
    else [uniq n (head disjs)] ++ (uniq'' n' (tail disjs)) where
        n' = n + (length $ getVariables $ head disjs)

uniq' :: CNF -> CNF
uniq' [] = []
uniq' cnf = 
    if (find (== []) cnf) /= Nothing then []
    else uniq'' 1 cnf

solve :: SSF -> Bool
solve (SSF quants [])     = True
solve (SSF quants matrix) = 
    if (find (== []) matrix) /= Nothing then False
    else solveCNF $ uniq' matrix