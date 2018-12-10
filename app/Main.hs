module Main where

import System.Environment (getArgs)

import Eval (eval)
import Parser (readExpr)

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
