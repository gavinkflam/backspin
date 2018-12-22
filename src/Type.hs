module Type
    (
      -- * Types
      LispVal(..)
    , LispError(..)
    , LispEnv
      -- * Monads
    , ThrowsError
    ) where

import Data.Array (Array, elems)
import Data.Complex (Complex(..), imagPart, realPart)
import Data.Ratio (numerator, denominator)

import Data.IORef (IORef)
import Text.ParserCombinators.Parsec (ParseError)

-- | Values and code structures for lisp.
data LispVal
    = Symbol String
    | Integer Integer
    | Rational Rational
    | Real Float
    | Complex (Complex Integer)
    | Character Char
    | String String
    | Boolean Bool
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Vector (Array Int LispVal)
    | PrimitiveFunc String
    | Func
        { params  :: [String]
        , vararg  :: Maybe String
        , body    :: [LispVal]
        , closure :: LispEnv
        }
    deriving (Eq)

instance Show LispVal where
    show = showVal

-- | Show the s-expression for LispVal.
showVal :: LispVal -> String
showVal (Symbol x)        = x
showVal (Integer x)       = show x
showVal (Rational x)      = show (numerator x) ++ "/" ++ show (denominator x)
showVal (Real x)          = show x
showVal (Complex x)       = show (realPart x) ++ "+" ++ show (imagPart x) ++ "i"
showVal (Character x)     = "#\\" ++ [x]
showVal (String x)        = "\"" ++ x ++ "\""
showVal (Boolean True)    = "#t"
showVal (Boolean False)   = "#f"
showVal (List xs)         = "(" ++ unwordsList xs ++ ")"
showVal (DottedList xs t) = "(" ++ unwordsList xs ++ " . " ++ showVal t ++ ")"
showVal (Vector xs)       = "#(" ++ unwordsList (elems xs) ++ ")"
showVal (PrimitiveFunc n) = "<primitive " ++ n ++ ">"
showVal fn@Func{}         =
    "(lambda (" ++ varargText ++ unwords (map show $ params fn) ++ ") ...)"
  where
    varargText = maybe "" (" . " ++) (vararg fn)

-- | Errors for evaluating or parsing lisp.
data LispError
    = NumArgs String Integer [LispVal]
    | TypeMismatch String String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | Default String
    deriving (Eq)

-- | Show `LispError` as text.
showError :: LispError -> String
showError (NumArgs fName expected found) =
    fName ++ ": Expected " ++ show expected ++ " arguments, found values " ++ vs
  where
    vs = unwords $ map show found
showError (TypeMismatch fName expected found) =
    fName ++ ": Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseError) = "Parse error at " ++ show parseError
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message fName)   = message ++ ": " ++ fName
showError (UnboundVar message varName)  = message ++ ": " ++ varName
showError (Default message)             = "Default: " ++ message

instance Show LispError where
    show = showError

-- | Monad for computation with potential `LispError`.
type ThrowsError = Either LispError

-- | Environment of mutable `LispVal` bindings.
type LispEnv = IORef [(String, IORef LispVal)]

-- | Join the s-expressions of a list of `LispVal` with spaces.
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
