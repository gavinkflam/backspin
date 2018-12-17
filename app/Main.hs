module Main where

import System.IO (putStrLn)

import Repl (runRepl)

main :: IO ()
main = putStrLn "Welcome to backspin REPL. Type `:quit` to exit." >> runRepl
