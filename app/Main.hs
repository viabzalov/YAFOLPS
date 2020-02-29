{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Main where

import Ast
import Parsers
import Text.Parsec
import Text.Pretty.Simple

main :: IO ()
main = print "Hello world!"
