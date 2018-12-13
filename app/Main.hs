module Main where

import System.Environment (getArgs)

import Eval (eval)
import Parser (readExpr)
import LispError (extractValue, trapError)

main :: IO ()
main = do
    args <- head <$> getArgs
    putStrLn $ extractValue $ trapError $ fmap show $ eval =<< readExpr args
