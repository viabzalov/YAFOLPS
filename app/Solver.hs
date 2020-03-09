{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Solver where

import           Ast
import           Ssf

import           Control.Monad
import           Data.Bool
import           Data.Foldable
import           Data.List
import           Data.Maybe

-- | Returns first Just from list if just exists, otherwise Nothing
firstJust :: [Maybe a] -> Maybe a
firstJust = join . find isJust

-- | Considers the dependence of the Term (second) on Variable (first)
dependsTermOnVariable :: Term -> Term -> Bool
dependsTermOnVariable (Variable name1) (Variable name2) = (name1 == name2)
dependsTermOnVariable vrb (FunctionSymbol (Symbol name args)) =
    or (fmap (dependsTermOnVariable vrb) args)
dependsTermOnVariable _ _ = False

-- dependsTermOnVariable (Variable "X") (FunctionSymbol (Symbol "f" [Variable "X"]))
-- answer : True

-- dependsTermOnVariable (Variable "X") (FunctionSymbol (Symbol "f" [Variable "Y"]))
-- answer: False

-- dependsTermOnVariable (Variable "Y") (Variable "X")
-- answer : False

-- dependsTermOnVariable (FunctionSymbol (Symbol "f" [Variable "X"])) (FunctionSymbol (Symbol "g" [Variable "Y"]))
-- answer : False

-- | Considers if two terms have almost the same structure
compareTwoTerms :: (Term, Term) -> Bool
compareTwoTerms ((Variable _ ), (Variable _)) = True
compareTwoTerms (vrb@(Variable name), trm) = not (dependsTermOnVariable vrb trm)
compareTwoTerms (trm, vrb@(Variable name)) = not (dependsTermOnVariable vrb trm)
compareTwoTerms ((FunctionSymbol (Symbol name1 args1)), (FunctionSymbol (Symbol name2 args2))) =
    if name1 /= name2 then False else
        if length args1 /= length args2 then False else
            and (fmap compareTwoTerms (zip args1 args2))

-- compareTwoTerms ((FunctionSymbol (Symbol "f" [Variable "X"])), (FunctionSymbol (Symbol "f" [Variable "Y"])))
-- answer : True

-- compareTwoTerms ((FunctionSymbol (Symbol "f" [Variable "X"])), (FunctionSymbol (Symbol "g" [Variable "Y"])))
-- answer : False

-- compareTwoTerms ((Variable "X"), (FunctionSymbol (Symbol "g" [Variable "Y"])))
-- answer : True

-- compareTwoTerms ((FunctionSymbol (Symbol "f" [(Variable "X"), (FunctionSymbol (Symbol "g" [Variable "Y"]))])), (FunctionSymbol (Symbol "f" [(Variable "Y"), (FunctionSymbol (Symbol "g" [Variable "Z"]))])))
-- answer : True

-- | Considers if Liters have almost the same structure
compareTwoLiters :: (Liter, Liter) -> Bool
compareTwoLiters ((PS smbl1), (NegPS smbl2)) = compareTwoLiters ((PS smbl1), (PS smbl2))
compareTwoLiters ((NegPS smbl1), (PS smbl2)) = compareTwoLiters ((PS smbl1), (PS smbl2))
compareTwoLiters ((NegPS smbl1), (NegPS smbl2)) = compareTwoLiters ((PS smbl1), (PS smbl2))
compareTwoLiters ((PS smbl1), (PS smbl2)) =
    compareTwoTerms ((FunctionSymbol smbl1), (FunctionSymbol smbl2))

-- | Returns first rename of variable in one pair of comparable terms, otherwise Nothing
getFirstRenameInPairOfTerms :: (Term, Term) -> Maybe (Term, Term)
getFirstRenameInPairOfTerms (vrb1@(Variable name1), vrb2@(Variable name2)) =
    if name1 /= name2 then Just $ (vrb1, vrb2) else Nothing
getFirstRenameInPairOfTerms (vrb1@(Variable name1), trm) =
    if (compareTwoTerms (vrb1, trm)) == True then Just $ (vrb1, trm) else Nothing
getFirstRenameInPairOfTerms (trm, vrb1@(Variable name1)) =
    if (compareTwoTerms (vrb1, trm)) == True then Just $ (vrb1, trm) else Nothing
getFirstRenameInPairOfTerms (trm1@(FunctionSymbol (Symbol name1 args1)), trm2@(FunctionSymbol (Symbol name2 args2))) =
    if (compareTwoTerms (trm1, trm2)) == True then firstJust (fmap getFirstRenameInPairOfTerms (zip args1 args2)) else Nothing

-- getFirstRenameInPairOfTerms ((FunctionSymbol (Symbol "f" [Variable "X"])), (FunctionSymbol (Symbol "f" [Variable "Y"])))
-- answer : Just (Variable "X",Variable "Y")

-- getFirstRenameInPairOfTerms ((FunctionSymbol (Symbol "f" [Variable "X"])), (FunctionSymbol (Symbol "g" [Variable "Y"])))
-- answer : Nothing

-- getFirstRenameInPairOfTerms ((Variable "X"), (FunctionSymbol (Symbol "g" [Variable "Y"])))
-- answer : Just (Variable "X",FunctionSymbol (Symbol {name = "g", args = [Variable "Y"]}))

-- getFirstRenameInPairOfTerms ((FunctionSymbol (Symbol "f" [(Variable "X"), (FunctionSymbol (Symbol "g" [Variable "Y"]))])), (FunctionSymbol (Symbol "f" [(Variable "Y"), (FunctionSymbol (Symbol "g" [Variable "Z"]))])))
-- answer : Just (Variable "X",Variable "Y")

-- getFirstRenameInPairOfTerms ((FunctionSymbol (Symbol "f" [(Variable "Y"), (FunctionSymbol (Symbol "g" [Variable "Y"]))])), (FunctionSymbol (Symbol "f" [(Variable "Y"), (FunctionSymbol (Symbol "g" [Variable "Z"]))])))
-- answer : Just (Variable "Y",Variable "Z")

-- | Returns first rename of variable in list comparable terms, otherwise Nothing
getFirstRenameInListOfPairOfTerms :: [(Term, Term)] -> Maybe (Term, Term)
getFirstRenameInListOfPairOfTerms [] = Nothing
getFirstRenameInListOfPairOfTerms (pr:prs) = case getFirstRenameInPairOfTerms pr of
    Nothing -> getFirstRenameInListOfPairOfTerms prs
    Just x  -> Just x

-- getFirstRenameInListOfPairOfTerms [(Variable "X", Variable "X"), (Variable "Z", Variable "Y")]
-- answer : Just (Variable "Z",Variable "Y")

-- | Returns first rename of variable in one pair of liters, otherwise Nothing
getFirstRenameInPairOfLiters :: (Liter, Liter) -> Maybe (Term, Term)
getFirstRenameInPairOfLiters ((PS smbl1), (NegPS smbl2)) = getFirstRenameInPairOfLiters ((PS smbl1), (PS smbl2))
getFirstRenameInPairOfLiters ((NegPS smbl1), (PS smbl2)) = getFirstRenameInPairOfLiters ((PS smbl1), (PS smbl2))
getFirstRenameInPairOfLiters ((NegPS smbl1), (NegPS smbl2)) = getFirstRenameInPairOfLiters ((PS smbl1), (PS smbl2))
getFirstRenameInPairOfLiters ((PS (Symbol name1 args1)), (PS (Symbol name2 args2))) = getFirstRenameInListOfPairOfTerms (zip args1 args2)

-- | Returns first rename of variable in list of liters, otherwise Nothing
getFirstRenameInListOfPairOfLiters :: [(Liter, Liter)] -> Maybe (Term, Term)
getFirstRenameInListOfPairOfLiters [] = Nothing
getFirstRenameInListOfPairOfLiters (pr:prs) = case getFirstRenameInPairOfLiters pr of
    Nothing -> getFirstRenameInListOfPairOfLiters prs
    Just x  -> Just x

-- | Returns first rename of variable in pair of disjuncts, otherwise Nothing
getFirstRenameInPairOfDisjuncts :: (Disjunct, Disjunct) -> Maybe (Term, Term)
getFirstRenameInPairOfDisjuncts (disj1, disj2) = getFirstRenameInListOfPairOfLiters (zip disj1 disj2)

-- | Returns first rename of variable in list of disjuncts, otherwise Nothing
getFristRenameInListOfPairOfDisjuncts :: [(Disjunct, Disjunct)] -> Maybe (Term, Term)
getFristRenameInListOfPairOfDisjuncts [] = Nothing
getFristRenameInListOfPairOfDisjuncts (pr:prs) = case getFirstRenameInPairOfDisjuncts pr of
    Nothing -> getFristRenameInListOfPairOfDisjuncts prs
    Just x  -> Just x

-- | Renames all occurrences of old_trm to new_trm in list of terms
renameTermInTerms :: [Term] -> (Term, Term) -> [Term]
renameTermInTerms [] _ = []
renameTermInTerms (trm@(Variable name):trms) pr@(old_trm, new_trm) = trm':trms' where
    trm' = if trm == old_trm then new_trm else trm
    trms' = renameTermInTerms trms pr
renameTermInTerms (trm@(FunctionSymbol (Symbol name args)):trms) pr@(old_trm, new_trm) = trm':trms' where
    trm' = if trm == old_trm then new_trm else FunctionSymbol (Symbol name (renameTermInTerms args pr))
    trms' = renameTermInTerms trms pr

-- renameTermInTerms [(Variable "X"), (FunctionSymbol (Symbol "p" [(Variable "X")]))] ((Variable "X"), (Variable "Y"))
-- answer : [Variable "Y",FunctionSymbol (Symbol {name = "p", args = [Variable "Y"]})]

-- renameTermInTerms [(FunctionSymbol (Symbol "f" [Variable "x"]))] ((FunctionSymbol (Symbol "f" [Variable "x"])), (FunctionSymbol (Symbol "g" [Variable "Y"])))
-- answer : [FunctionSymbol (Symbol {name = "g", args = [Variable "Y"]})]

-- | Renames all occurrences of old_trm to new_trm in one liter
renameTermInLiter :: Liter -> (Term, Term) -> Liter
renameTermInLiter (PS (Symbol name args)) pr = (PS (Symbol name (renameTermInTerms args pr)))
renameTermInLiter (NegPS (Symbol name args)) pr = (NegPS (Symbol name (renameTermInTerms args pr)))

-- renameTermInLiter (PS (Symbol "p" [(FunctionSymbol (Symbol "f" [Variable "x"]))])) ((FunctionSymbol (Symbol "f" [Variable "x"])), (FunctionSymbol (Symbol "g" [Variable "Y"])))
-- answer : PS {getPS = Symbol {name = "p", args = [FunctionSymbol (Symbol {name = "g", args = [Variable "Y"]})]}}

-- | Renames all occurrences of old_trm to new_trm in list of liters
renameTermInLiters :: [Liter] -> (Term, Term) -> [Liter]
renameTermInLiters [] _ = []
renameTermInLiters (ltr:ltrs) pr = ltr':ltrs' where
    ltr' = renameTermInLiter ltr pr
    ltrs' = renameTermInLiters ltrs pr

-- | Renames all occurrences of old_trm to new_trm in one disjunct
renameTermInDisjunct :: Disjunct -> (Term, Term) -> Disjunct
renameTermInDisjunct = renameTermInLiters

-- | Renames all occurrences of old_trm to new_trm in list of disjuncts
renameTermInListOfDisjuncts :: [Disjunct] -> (Term, Term) -> [Disjunct]
renameTermInListOfDisjuncts [] _ = []
renameTermInListOfDisjuncts (disj:disjs) pr = disj':disjs' where
    disj' = renameTermInDisjunct disj pr
    disjs' = renameTermInListOfDisjuncts disjs pr

-- | Return list of variables used in list of terms
getVariablesOfTerms :: [Term] -> [Term] -> [Term]
getVariablesOfTerms [] ans = ans
getVariablesOfTerms ((Variable name):trms) ans =
    union [Variable name] (getVariablesOfTerms trms ans)
getVariablesOfTerms ((FunctionSymbol (Symbol name args)):trms) ans =
    union (getVariablesOfTerms args ans) (getVariablesOfTerms trms ans)

-- | Return list of variables used in list of liters
getVariablesOfLiters :: [Liter] -> [Term] -> [Term]
getVariablesOfLiters [] ans = ans
getVariablesOfLiters ((PS (Symbol name args)):ltrs) ans =
    union (getVariablesOfTerms args ans) (getVariablesOfLiters ltrs ans)
getVariablesOfLiters ((NegPS (Symbol name args)):ltrs) ans =
    union (getVariablesOfTerms args ans) (getVariablesOfLiters ltrs ans)

-- | Return list of variables used in disjunct
getVariablesOfDisjunct :: Disjunct -> [Term] -> [Term]
getVariablesOfDisjunct = getVariablesOfLiters

-- | Renames all variables in disjunct to special kind; with start number
uniqueVariablesInDisjunct' :: Disjunct -> Int -> [Term] -> Disjunct
uniqueVariablesInDisjunct' disj _ [] = disj
uniqueVariablesInDisjunct' disj n (vrb:vrbs) =
    uniqueVariablesInDisjunct' (renameTermInDisjunct disj (vrb, Variable $ "X_" ++ show n)) (n + 1) (vrbs)

-- | Renames all variables in CNF to special kind; with start number
uniqueVariablesInCNF' :: CNF -> Int -> CNF
uniqueVariablesInCNF' [] _ = []
uniqueVariablesInCNF' (disj:disjs) n = disj':disjs' where
    vrbs = getVariablesOfDisjunct disj []
    len = length vrbs
    disj' = uniqueVariablesInDisjunct' disj n vrbs
    disjs' = uniqueVariablesInCNF' disjs (n + len)

-- | Renames all variables in CNF to special kind
uniqueVariablesInCNF :: CNF -> CNF
uniqueVariablesInCNF cnf = uniqueVariablesInCNF' cnf 1

-- | Renames all variables in SSF to special kind
uniqueVariablesInSSF :: SSF -> SSF
uniqueVariablesInSSF (SSF quants matrix) = (SSF quants (uniqueVariablesInCNF matrix))

-- | Unify two terms into one, if can, otherwise Nothing
unifyTwoTerms :: Term -> Term -> Maybe Term
unifyTwoTerms (Variable name1) (Variable name2) = Just $ Variable $ name1
unifyTwoTerms vrb@(Variable name1) trm =
    if (dependsTermOnVariable vrb trm) == False then Just $ trm else Nothing
unifyTwoTerms trm vrb@(Variable name1) =
    if (dependsTermOnVariable vrb trm) == False then Just $ trm else Nothing
unifyTwoTerms trm1 trm2 =
    if trm1 == trm2 then Just $ trm1 else
        if (compareTwoTerms (trm1, trm2)) == False then Nothing else
            if pr == Nothing then Just $ trm1 else unifyTwoTerms trm1' trm2' where
                trm1' = head (renameTermInTerms [trm1] pr')
                trm2' = head (renameTermInTerms [trm2] pr')
                pr = getFirstRenameInPairOfTerms (trm1, trm2)
                pr' = case pr of
                    Nothing -> (Variable "X", Variable "X")
                    Just x  -> x

-- unifyTwoTerms (Variable "X") (Variable "Y")
-- answer : Just (Variable "X")

-- unifyTwoTerms (FunctionSymbol (Symbol "f" [Variable "X"])) (FunctionSymbol (Symbol "f" [Variable "Y"]))
-- answer : Just (FunctionSymbol (Symbol {name = "f", args = [Variable "Y"]}))

-- unifyTwoTerms (FunctionSymbol (Symbol "f" [Variable "X"])) (FunctionSymbol (Symbol "g" [Variable "Y"]))
-- answer : Nothing

-- unifyTwoTerms (FunctionSymbol (Symbol "f" [(Variable "X"), (FunctionSymbol (Symbol "g" [Variable "Y"]))])) (FunctionSymbol (Symbol "f" [(Variable "Y"), (FunctionSymbol (Symbol "g" [Variable "Z"]))]))
-- answer : Just (FunctionSymbol (Symbol {name = "f", args = [Variable "Z",FunctionSymbol (Symbol {name = "g", args = [Variable "Z"]})]}))

-- unifyTwoTerms (FunctionSymbol (Symbol "f" [(FunctionSymbol (Symbol "f" [Variable "X"]))])) (FunctionSymbol (Symbol "f" [(FunctionSymbol (Symbol "g" [Variable "X"]))]))
-- answer : Nothing

-- | Unify two contrary liters
unifyTwoLiters' :: Liter -> Liter -> Maybe Liter
unifyTwoLiters' (PS smbl1) (NegPS smbl2) = unifyTwoLiters (PS smbl1) (PS smbl2)
unifyTwoLiters' (NegPS smbl1) (PS smbl2) = unifyTwoLiters (PS smbl1) (PS smbl2)
unifyTwoLiters' _ _ = Nothing

-- | Unify two almost contrary liters
unifyTwoLiters :: Liter -> Liter -> Maybe Liter
unifyTwoLiters (PS smbl1) (NegPS smbl2) = unifyTwoLiters (PS smbl1) (PS smbl2)
unifyTwoLiters (NegPS smbl1) (PS smbl2) = unifyTwoLiters (PS smbl1) (PS smbl2)
unifyTwoLiters (NegPS smbl1) (NegPS smbl2) = unifyTwoLiters (PS smbl1) (PS smbl2)
unifyTwoLiters ltr1@(PS smbl1@(Symbol name1 args1)) ltr2@(PS smbl2@(Symbol name2 args2)) =
    if ltr1 == ltr2 then Just $ ltr1 else
        if (name1 /= name2) || (and (fmap compareTwoTerms (zip args1 args2)) == False) then Nothing else
            if pr == Nothing then Just $ ltr1 else unifyTwoLiters ltr1' ltr2' where
                ltr1' = renameTermInLiter ltr1 pr'
                ltr2' = renameTermInLiter ltr2 pr'
                pr = getFirstRenameInListOfPairOfTerms (zip args1 args2)
                pr' = case pr of
                    Nothing -> (Variable "X", Variable "X")
                    Just x  -> x

-- unifyTwoLiters (PS (Symbol "p" [Variable "X"])) (PS (Symbol "p" [Variable "Y"])) --> Just (PS {getPS = Symbol {name = "p", args = [Variable "Y"]}})
-- unifyTwoLiters (PS (Symbol "p" [Variable "X"])) (PS (Symbol "q" [Variable "Y"])) --> Nothing

-- | Rename disjunct by renames of two unifiable liters
renameDisjunctByTwoLiters :: Disjunct -> (Liter, Liter) -> Disjunct
renameDisjunctByTwoLiters [] _ = []
renameDisjunctByTwoLiters disj (ltr1, ltr2) =
    if ltr1 == ltr2 then disj else ans where
        ans = renameDisjunctByTwoLiters (disj') (ltr1', ltr2')
        disj' = renameTermInDisjunct disj pr'
        ltr1' = renameTermInLiter ltr1 pr'
        ltr2' = renameTermInLiter ltr2 pr'
        pr = getFirstRenameInPairOfLiters (ltr1, ltr2)
        pr' = case pr of
                Nothing -> (Variable "X", Variable "X")
                Just x  -> x

-- | Unify two disjuncts
unifyTwoDisjuncts :: (Disjunct, Disjunct) -> Maybe (Disjunct, Maybe (Liter, Liter))
unifyTwoDisjuncts ([], []) = Nothing
unifyTwoDisjuncts ([], _ ) = Nothing
unifyTwoDisjuncts (_,  []) = Nothing
unifyTwoDisjuncts (disj1@(ltr1:ltrs1), disj2@(ltr2:ltrs2)) =
    if disj1 == disj2 then Just $ (disj1, Nothing) else
        case (unifyTwoLiters' ltr1 ltr2) of
            Just x -> Just $ (renameDisjunctByTwoLiters (ltrs1 ++ ltrs2) (ltr1, ltr2), Just $ (ltr1, ltr2))
            Nothing -> case (unifyTwoDisjuncts (ltrs1, disj2)) of
                Just (x, Just pr) -> Just $ (renameDisjunctByTwoLiters (ltr1:x) pr, Just $ pr)
                Nothing -> case (unifyTwoDisjuncts (disj1, ltrs2)) of
                    Just (x, Just pr)  -> Just $ (renameDisjunctByTwoLiters (ltr2:x) pr, Just $ pr)
                    otherwise -> Nothing
                otherwise -> Nothing

-- | Make new CNF by unify some of two disjuncts of CNF
newCNF :: CNF -> [(Disjunct, Disjunct)] -> CNF
newCNF cnf [] = cnf
newCNF cnf ((disj1, disj2):prs) =
    case (unifyTwoDisjuncts (disj1, disj2)) of
        Just (x, Just pr) -> (delete disj1 (delete disj2 cnf)) `union` [x]
        otherwise         -> newCNF cnf prs

-- | Remove all empty disjuncts
normalizeCNF :: CNF -> CNF
normalizeCNF [] = []
normalizeCNF (disj:disjs) =
    if disj == [] then cnf' else disj:cnf' where
        cnf' = normalizeCNF disjs

-- | Solve CNF by resolution method
solveCNF :: CNF -> Bool
solveCNF [] = False
solveCNF cnf = if cnf == cnf' then True else (solveCNF cnf') where
    cnf' = normalizeCNF (newCNF cnf prs)
    prs = [(x, y) | x <- cnf, y <- cnf, x /= y]

-- | Solve a theorem represented by its SSF
solve :: SSF -> Bool
solve (SSF quants [])     = True
solve (SSF quants matrix) = solveCNF (uniqueVariablesInCNF matrix)
