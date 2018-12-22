module Repl
    (
      -- * Repl
      runRepl
    ) where

import System.IO (getLine, hFlush, putStr, putStrLn, stdout)
import Control.Monad.Except (liftEither)

import Env (newLispEnv)
import Error (runIOThrows)
import Eval (eval)
import Parser (readExpr)
import Type (LispEnv)

-- | Run repl session that exit with the command `:quit`.
runRepl :: IO ()
runRepl =
    until_ (== ":quit") (readPrompt "Backspin>>> ") . evalAndPrint =<< newLispEnv

-- | Print the prompt message and get input from user.
readPrompt :: String -> IO String
readPrompt prompt = putStr prompt >> hFlush stdout >> getLine

-- | Evaluate the expression and print the output.
evalAndPrint :: LispEnv -> String -> IO ()
evalAndPrint env expr = putStrLn =<< runIOThrows
    (show <$> (eval env =<< liftEither (readExpr expr)))

-- | Continue to get input from `prompt` and execute `action`, until `cond`
--   was met.
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ cond prompt action = do
    result <- prompt
    if cond result
       then return ()
       else action result >> until_ cond prompt action
