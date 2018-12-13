module LispError
    (
      -- * Types
      LispError(..)
    , ThrowsError
      -- * Utility
    , extractValue
    , trapError
    ) where

import Control.Monad.Except (MonadError, catchError)
import Text.ParserCombinators.Parsec (ParseError)

import LispVal (LispVal)

-- | Errors for evaluating or parsing lisp.
data LispError
    = NumArgs String Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | Default String
    deriving (Eq)

instance Show LispError where
    show = showError

-- | Monad for computation with potential `LispError`.
type ThrowsError = Either LispError

-- | Catch errors and display error text if any.
trapError :: (Show a, MonadError a m) => m String -> m String
trapError action = catchError action (return . show)

-- | Extract right value from `ThrowsError` monad.
extractValue :: ThrowsError a -> a
extractValue (Left m)  = error $ "extractValue: " ++ show m
extractValue (Right v) = v

-- | Show `LispError` as text.
showError :: LispError -> String
showError (NumArgs fName expected found) =
    fName ++ ": Expected " ++ show expected ++ " arguments, found values " ++ vs
  where
    vs = unwords $ map show found
showError (TypeMismatch expected found) =
    "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseError) = "Parse error at " ++ show parseError
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message fName)   = message ++ ": " ++ fName
showError (UnboundVar message varName)  = message ++ ": " ++ varName
showError (Default message)             = "Default: " ++ message
