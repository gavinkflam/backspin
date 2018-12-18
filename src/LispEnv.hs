module LispEnv
    (
      -- * Types
      LispEnv
      -- * Initialization
    , newLispEnv
      -- * Read
    , isBound
    , getVar
      -- * Write
    , setVar
    ) where

import Control.Monad.Except (liftIO, throwError)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)

import LispError (IOThrowsError, LispError(..))
import LispVal (LispVal)

-- | Environment of mutable `LispVal` bindings.
type LispEnv = IORef [(String, IORef LispVal)]

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
