{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Main where

import Parsers
import ConverterToSSF
import Text.Parsec
import Text.Pretty.Simple
import System.Environment
import Data.Maybe
import Data.List

main :: IO ()
main = do
    
    args <- getArgs

    formula <- case getInputFile args of
        Nothing -> do
            putStrLn "Enter a formula:"
            formula <- getLine
            putStrLn ""
            return formula
        Just nameFile ->
            readFile nameFile
    
    if getIsPrintedFormula args then do
        putStrLn "Entered formula:"
        print formula
        putStrLn ""
    else return ()
    
    ast <- case parse parseFormula "" formula of
        Left err -> do
            print err
            fail ""
        Right ast -> return ast
    
    if getIsPrintedAST args then do
        putStrLn "Obtained AST:"
        pPrint ast
        putStrLn ""
    else return ()

    let ssf = convertToSSF ast

    if getIsPrintedSSF args then do
        putStrLn "Converted SSF:"
        pPrint ssf
        putStrLn ""
    else return ()
    
    where
        getInputFile :: [String] -> Maybe FilePath
        getInputFile args =
            case findIndex (\arg -> arg == "--i" || arg == "--input-file") args of
                Nothing -> Nothing
                Just ind ->
                    if ind + 1 == length args then
                        error "File was not defined"
                    else
                        Just (args !! (ind + 1))
        
        getBoolFromArgs :: String -> Char -> [String] -> Bool
        getBoolFromArgs nameOption charOption args = isJust $
            find
                (\arg ->
                    (
                        length arg > 1 &&
                        head arg == '-' &&
                        head (tail arg) /= '-' &&
                        isJust (find (== charOption) arg)
                    ) ||
                    arg == nameOption
                )
                args
        
        getIsPrintedFormula :: [String] -> Bool
        getIsPrintedFormula = getBoolFromArgs "--print-formula" 'f'
        
        getIsPrintedAST :: [String] -> Bool
        getIsPrintedAST = getBoolFromArgs "--print-ast" 'a'
        
        getIsPrintedSSF :: [String] -> Bool
        getIsPrintedSSF = getBoolFromArgs "--print-ssf" 's'
