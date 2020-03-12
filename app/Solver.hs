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
import qualified Data.Tuple         as T

import           Debug.Pretty.Simple

type Substitution = [(Term, Term)]

firstJust :: [Maybe a] -> Maybe a
firstJust = join . find isJust

composition :: Substitution -> Substitution -> Substitution
composition s1 s2 = unique $ s1' ++ s2' where
    (vs1, ts1) = unzip s1
    (vs2, ts2) = unzip s2
    tmp = zip vs1 ts2
    s1' = zip vs1 (map (apply s2) ts1)
    s2' = [x | x <- s2, not $ x `elem` tmp]

unique :: Substitution -> Substitution
unique s = [x | x@(fi, se) <- s, fi /= se]

decompose :: Substitution -> Maybe Substitution
decompose [] = Just $ []
decompose (p:ps) =
    case p of
        (f1@(FunctionSymbol s1), f2@(FunctionSymbol s2)) ->
            if (equiv f1 f2)
                then (decompose $ (zip (args s1) (args s2)) ++ ps)
            else Nothing
        otherwise ->
            case decompose ps of
                Just ps' -> Just $ p:ps'
                Nothing  -> Nothing

swap :: Substitution -> Substitution
swap [] = []
swap (p:ps) =
    case p of
        (f@(FunctionSymbol s), v@(Variable name)) -> (v, f):(swap ps)
        otherwise                                 -> p:(swap ps)

eliminate :: Substitution -> Substitution -> Substitution
eliminate [] s = s
eliminate (p:ps) s =
    case p of
        (v@(Variable name), trm) ->
            if (occursCheck [p] /= Nothing) && (v `elem` vs)
                then eliminate s' s'
            else eliminate ps s where
                s'' = delete p s
                (ls, rs) = unzip s''
                vs = foldl (union) [] ((map getVariables ls) ++ (map getVariables rs))
                replace' :: (Term, Term) -> (Term, Term) -> (Term, Term)
                replace' p (fi, se) = ((apply [p] fi), (apply [p] se))
                s' = p:(map (replace' p) s'')
        otherwise -> eliminate ps s

occursCheck :: Substitution -> Maybe Substitution
occursCheck s =
    if and $ map check s
        then Just $ s
    else Nothing where
        check :: (Term, Term) -> Bool
        check (v, t) = not $ v `elem` (getVariables t)

unify' :: Substitution -> Maybe Substitution
unify' s =
    case decompose s of
        Nothing -> Nothing
        Just s1 ->
            case occursCheck (swap s1) of
                Nothing -> Nothing
                Just s2 ->
                    if isSubstitution s3
                        then Just $ s3
                    else unify' s3 where
                        s3 = unique $ swap $ eliminate s2 s2

isSubstitution :: Substitution -> Bool
isSubstitution s = ans where

    isVariables :: [Term] -> Bool
    isVariables [] = True
    isVariables (t:ts) =
        case t of
            (Variable name) -> True && (isVariables ts)
            otherwise       -> False

    (ls, rs) = unzip s
    rVariables = foldl (union) [] (map getVariables rs)

    ans = (isVariables ls) && (and $ [not (x `elem` rVariables) | x <- ls])

class Unifiable a where
    getVariables :: a -> [Term]
    apply :: Substitution -> a -> a
    equiv :: a -> a -> Bool
    unify :: a -> a -> Maybe [Substitution]
    uniq :: Int -> a -> a

instance Unifiable Term where
    getVariables (FunctionSymbol smbl) = nub $ concatMap getVariables (args smbl)
    getVariables vrb                   = [vrb]

    apply sbstn vrb@(Variable name)  =
        case lookup vrb sbstn of
            Just trm  -> trm
            otherwise -> vrb
    apply sbstn (FunctionSymbol smbl) =
        FunctionSymbol $ Symbol (name smbl) (map (apply sbstn) (args smbl))

    equiv (Variable name1) (Variable name2) = True 
    equiv v@(Variable name) f@(FunctionSymbol smbl) = not $ v `elem` (getVariables f)
    equiv f@(FunctionSymbol smbl) v@(Variable name) = not $ v `elem` (getVariables f)
    equiv (FunctionSymbol smbl1) (FunctionSymbol smbl2) =
        (name smbl1) == (name smbl2) &&
        (length (args smbl1)) == (length (args smbl2)) &&
        and (map (uncurry equiv) (zip (args smbl1) (args smbl2)))

    unify trm1 trm2 =
        case unify' [(trm1, trm2)] of
            Nothing    -> Nothing
            Just sbstn -> Just $ [sbstn]

    uniq n trm = trm' where
        vrbs = getVariables trm
        vrbs' = map (Variable . show) [n..(n + length vrbs)]
        trm' = apply (zip vrbs vrbs') trm

instance Unifiable Liter where
    getVariables ltr = getVariables $ FunctionSymbol $ getPS $ ltr

    apply sbstn (PS smbl) = PS $ Symbol (name smbl) (map (apply sbstn) (args smbl))
    apply sbstn (NegPS smbl) = NegPS $ Symbol (name smbl) (map (apply sbstn) (args smbl))

    equiv ltr1 ltr2 = equiv (FunctionSymbol $ getPS $ ltr1) (FunctionSymbol $ getPS $ ltr2)

    unify ltr1 ltr2 = unify (FunctionSymbol $ getPS $ ltr1) (FunctionSymbol $ getPS $ ltr2)

    uniq n ltr = ltr' where
        vrbs = getVariables ltr
        vrbs' = map (Variable . show) [n..(n + length vrbs)]
        ltr' = apply (zip vrbs vrbs') ltr

instance Unifiable Disjunct where
    getVariables disj = nub $ concatMap getVariables disj

    apply sbstn disj = map (apply sbstn) disj

    equiv disj1 disj2 = and $ map (uncurry equiv) (zip disj1 disj2)

    unify disj1 disj2 =
        if (l1 == []) && (l2 == []) then Nothing
        else Just $ nub $ l1 ++ l2 where
            l1 = concat $ catMaybes [unify x y | x@(PS s1) <- disj1, y@(NegPS s2) <- disj2]
            l2 = concat $ catMaybes [unify x y | x@(NegPS s1) <- disj1, y@(PS s2) <- disj2]

    uniq n disj = disj' where
        vrbs = getVariables disj
        vrbs' = map (Variable . show) [n..(n + length vrbs)]
        disj' = apply (zip vrbs vrbs') disj

resolution :: Disjunct -> Disjunct -> [Disjunct]
resolution disj1 disj2 = 
    --pTrace ("RESOLUTION :\n" ++ (show disj1) ++ " # " ++ (show disj2) ++ " #$# " ++ (show $ nub $ l1 ++ l2)) $ 
    nub $ l1 ++ l2 where
    l1 =
        [ delete x' $ delete y' $ nub (disj1' ++ disj2')
          | x@(PS s1) <- disj1,
            y@(NegPS s2) <- disj2,
            s' <- fromMaybe [] (unify x y),
            let disj1' = apply s' disj1,
            let disj2' = apply s' disj2,
            let x' = apply s' x,
            let y' = apply s' y
        ]
    l2 =
        [ delete x' $ delete y' $ nub (disj1' ++ disj2')
          | x@(NegPS s1) <- disj1,
            y@(PS s2) <- disj2,
            (name s1) == (name s2),
            s' <- fromMaybe [] (unify x y),
            let disj1' = apply s' disj1,
            let disj2' = apply s' disj2,
            let x' = apply s' x,
            let y' = apply s' y
        ]

gluing :: Disjunct -> [Disjunct]
gluing disj = ans where
    l1 = concat $ catMaybes [unify x y | x@(PS s1) <- disj, y@(PS s2) <- disj, x /= y, (name s1) == (name s2)]
    l2 = concat $ catMaybes [unify x y | x@(NegPS s1) <- disj, y@(NegPS s2) <- disj, x /= y, (name s1) == (name s2)]
    l = l1 ++ l2
    ans = [nub $ apply x disj | x <- l]

solveCNF :: CNF -> Bool
solveCNF [] = True
solveCNF cnf = -- pTrace ("SOLVECNF :\n" ++ show cnf) $
    if (find (== []) res /= Nothing) then False
    else if cnf == res then True
    else solveCNF res where
        cnf1 = concat [resolution x y | x <- cnf, y <- cnf, x /= y]
        cnf2 = concat [resolution x' y | x <- cnf, y <- cnf, x /= y, x' <- gluing x]
        cnf3 = concat [resolution x y' | x <- cnf, y <- cnf, x /= y, y' <- gluing y]
        cnf4 = concat [resolution x' y' | x <- cnf, y <- cnf, x /= y, x' <- gluing x, y' <- gluing y]
        res = uniq' $nub $ cnf ++ cnf1 ++ cnf2 ++ cnf3 ++ cnf4

uniq'' :: Int -> CNF -> CNF
uniq'' n disjs =
    if length disjs == 1 then [uniq n (head disjs)]
    else [uniq n (head disjs)] ++ (uniq'' n' (tail disjs)) where
        n' = n + (length $ getVariables $ head disjs)

uniq' :: CNF -> CNF
uniq' [] = []
uniq' cnf =
    if (find (== []) cnf /= Nothing) then [[]]
    else uniq'' 1 cnf

solve :: SSF -> Bool
solve (SSF quants [])     = True
solve (SSF quants matrix) = solveCNF $ uniq' matrix
