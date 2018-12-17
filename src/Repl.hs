module Repl
    (
      -- * Repl
      runRepl
    ) where

import System.IO (getLine, hFlush, putStr, putStrLn, stdout)

import Eval (eval)
import LispError (extractValue, trapError)
import Parser (readExpr)

-- | Run repl session that exit with the command `:quit`.
runRepl :: IO ()
runRepl = until_ (== ":quit") (readPrompt "Backspin>>> ") evalAndPrint

-- | Print the prompt message and get input from user.
readPrompt :: String -> IO String
readPrompt prompt = putStr prompt >> hFlush stdout >> getLine

-- | Evaluate the expression and print the output.
evalAndPrint :: String -> IO ()
evalAndPrint expr =
    putStrLn $ extractValue $ trapError $ show <$> (eval =<< readExpr expr)

-- | Continue to get input from `prompt` and execute `action`, until `cond`
--   was met.
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ cond prompt action = do
    result <- prompt
    if cond result
       then return ()
       else action result >> until_ cond prompt action
