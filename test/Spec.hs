import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.List
import           Data.Ord

import           Ast
import           ConverterToSSF
import           Parsers
import           Solver
import           Ssf

import           Text.Parsec

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

test :: String -> Either ParseError Bool
test s = solve <$> convertToSSF <$> Neg <$> parse parseFormula "" s

unitTests = testGroup "Unit tests"
    [
        testCase "Law 1: a>(b>a)" $
        test "a>(b>a)" @?= Right False
    ,
        testCase "Law 2: (a>(b>c))>((a>b)>(a>c))" $
        test "(a>(b>c))>((a>b)>(a>c))" @?= Right False
    ,
        testCase "Law 3: a>(b>(a&b))" $
        test "a>(b>(a&b))" @?= Right False
    ,
        testCase "Law 4: (a&b)>a" $
        test "(a&b)>a" @?= Right False
    ,
        testCase "Law 5: (a&b)>b" $
        test "(a&b)>b" @?= Right False
    ,
        testCase "Law 6: (a>c)>((b>c)>((a|b)>c))" $
        test "(a>c)>((b>c)>((a|b)>c))" @?= Right False
    ,
        testCase "Law 7: a>(a|b)" $
        test "a>(a|b)" @?= Right False
    ,
        testCase "Law 8: b>(a|b)" $
        test "b>(a|b)" @?= Right False
    ,
        testCase "Law 9: (a>b)>((a>-b)>-a)" $
        test "(a>b)>((a>-b)>-a)" @?= Right False
    ,
        testCase "Law 10: --a>a" $
        test "--a>a" @?= Right False
    ,
        testCase "Law 11: _>a" $
        test "_>a" @?= Right False
    ,
        testCase "Law 12: a>^" $
        test "a>^" @?= Right False
    ,
        testCase "Law 13: (!X(p(X)))>(p(Y))" $
        test "(!X(p(X)))>(p(Y))" @?= Right False
    ,
        testCase "Law 14: (p(Y))>(?X(p(X)))" $
        test "(p(Y))>(?X(p(X)))" @?= Right False
    ,
        testCase "Law 15: (!X(q(Y)>p(X)))>((q(Y))>(!X(p(X))))" $
        test "(!X(q(Y)>p(X)))>((q(Y))>(!X(p(X))))" @?= Right False
    ,
        testCase "Law 16: (!X(p(X)>q(Y)))>((?X(p(X)))>(q(Y)))" $
        test "(!X(p(X)>q(Y)))>((?X(p(X)))>(q(Y)))" @?= Right False
    ]
