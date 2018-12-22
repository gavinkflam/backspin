module Error
    (
      -- * Monads
      IOThrowsError
      -- * IO
    , runIOThrows
    ) where

import Control.Monad.Except (ExceptT, MonadError, catchError, runExceptT)

import Type (LispError(..), ThrowsError)

-- | Add `LispError` exception to IO.
type IOThrowsError = ExceptT LispError IO

-- | Run `IOThrowsError` into IO
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runExceptT (trapError action)

-- | Catch errors and display error text if any.
trapError :: (Show a, MonadError a m) => m String -> m String
trapError action = catchError action (return . show)

-- | Extract right value from `ThrowsError` monad.
extractValue :: ThrowsError a -> a
extractValue (Left m)  = error $ "extractValue: " ++ show m
extractValue (Right v) = v
