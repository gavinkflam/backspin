module Main where

import System.Environment (getArgs)

import Expr (readExpr)

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn $ readExpr expr
