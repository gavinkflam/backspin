module Env
    (
      -- * Initialization
      newLispEnv
      -- * Read
    , getVar
      -- * Write
    , setVar
    , defineVar
    ) where

import Control.Monad.Except (liftIO, throwError)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)

import Error (IOThrowsError)
import Type (LispEnv, LispError(..), LispVal)

-- | Build a new empty LispEnv.
newLispEnv :: IO LispEnv
newLispEnv = newIORef []

-- | Determine if a variable is bounded in the given environment.
isBound :: LispEnv -> String -> IO Bool
isBound envRef name = isJust . lookup name <$> readIORef envRef

-- | Read the `LispVal` of a variable bounded to the given environment.
getVar :: LispEnv -> String -> IOThrowsError LispVal
getVar envRef name =
    maybe
    (throwError $ UnboundVar "Getting an unbound variable" name)
    (liftIO . readIORef)
    <$> lookup name =<< liftIO (readIORef envRef)

-- | Update a variable bounded to the given environment.
setVar :: LispEnv -> String -> LispVal -> IOThrowsError LispVal
setVar envRef name val =
    (maybe
    (throwError $ UnboundVar "Setting an unbound variable" name)
    (liftIO . (`writeIORef` val))
    <$> lookup name =<< liftIO (readIORef envRef)) >> return val

-- | Define a new variable or update an bounded variable.
defineVar :: LispEnv -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef name val = do
    bound <- liftIO $ isBound envRef name
    if bound
        then setVar envRef name val
        else liftIO $ do
            valRef <- newIORef val
            env    <- readIORef envRef
            writeIORef envRef ((name, valRef) : env)
            return val
