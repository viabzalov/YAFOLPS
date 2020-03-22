{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Main where

import           Ast
import           ConverterToSSF
import           Data.Function
import           Data.List
import           Data.Maybe
import           Parsers
import           Solver
import           System.Console.GetOpt
import           System.Environment
import           Text.Parsec
import           Text.Pretty.Simple

data Options = Options
    {
        inputFile        :: Maybe FilePath,
        isPrintedFormula :: Bool,
        isPrintedAST     :: Bool,
        isPrintedSSF     :: Bool,
        isPrintedResult  :: Bool
    }

defaultOptions :: Options
defaultOptions = Options
    {
        inputFile        = Nothing,
        isPrintedFormula = False,
        isPrintedAST     = False,
        isPrintedSSF     = False,
        isPrintedResult  = False
    }

options :: [OptDescr (Options -> Options)]
options =
    [
        Option
            ['i']
            ["input-file"]
            (ReqArg
                (\path opts -> opts { inputFile = Just path })
                "FILE"
            )
            "file with formula",
        Option
            ['f']
            ["print-formula"]
            (NoArg (\opts -> opts { isPrintedFormula = True }))
            "print formula",
        Option
            ['a']
            ["print-ast"]
            (NoArg (\opts -> opts { isPrintedAST = True }))
            "print AST",
        Option
            ['s']
            ["print-ssf"]
            (NoArg (\opts -> opts { isPrintedSSF = True }))
            "print SSF",
        Option
            ['r']
            ["print-solution"]
            (NoArg (\opts -> opts { isPrintedResult = True }))
            "print solution"
    ]

getOpts :: [String] -> IO Options
getOpts args =
    case getOpt Permute options args of
        (o, _, [])  -> return $ foldl (&) defaultOptions o
        (_, _, err) -> ioError $ userError $ concat err

main :: IO ()
main = do

    args <- getArgs

    opts <- getOpts args

    formula <- case opts & inputFile of
        Nothing -> do
            putStrLn "Enter a formula:"
            formula <- getLine
            putStrLn ""
            return formula
        Just nameFile ->
            readFile nameFile

    if opts & isPrintedFormula then do
        putStrLn "Entered formula:"
        print formula
        putStrLn ""
    else return ()

    ast <- case parse parseFormula "" formula of
        Left err -> do
            print err
            fail ""
        Right ast -> return ast

    if opts & isPrintedAST then do
        putStrLn "Obtained AST:"
        pPrint ast
        putStrLn ""
    else return ()

    let ssf = convertToSSF (Neg ast)

    if opts & isPrintedSSF then do
        putStrLn "Converted SSF:"
        pPrint ssf
        putStrLn ""
    else return ()

    let result = if (solve $ convertToSSF $ Neg $ ast) == True then "NOT VALID" else "VALID"

    if opts & isPrintedResult then do
        putStrLn "FOL Solver result:"
        pPrint result
        putStrLn ""
    else return ()

