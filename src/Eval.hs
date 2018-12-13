module Eval
    (
      -- * Evaluate
      eval
    ) where

import Control.Monad.Except (throwError)

import LispVal (LispVal(..))
import LispError (LispError(..), ThrowsError)

-- | Evaluate `LispVal` recursively.
eval :: LispVal -> ThrowsError LispVal
eval x@(Integer _)   = return x
eval x@(Rational _)  = return x
eval x@(Real _)      = return x
eval x@(Complex _)   = return x
eval x@(Character _) = return x
eval x@(String _)    = return x
eval x@(Boolean _)   = return x
eval (List [Symbol "quote", x])   = return x
eval (List (Symbol fName : args)) = apply fName =<< mapM eval args
eval x               = throwError $ BadSpecialForm "Unrecognized special form" x

-- | Apply function by name and arguments.
apply :: String -> [LispVal] -> ThrowsError LispVal
apply fName args =
    maybe
    (throwError $ NotFunction "Unrecognized primitive function " fName)
    ($ args) $ lookup fName primitives

-- | Primitive functions for lisp.
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
    -- Numerical operators
    [ ("+",          numericBinop "+" (+))
    , ("-",          numericBinop "-" (-))
    , ("*",          numericBinop "*" (*))
    , ("/",          numericBinop "div" div)
    , ("mod",        numericBinop "mod" mod)
    , ("quotient",   numericBinop "quot" quot)
    , ("remainder",  numericBinop "rem" rem)
    -- Type checking predicates
    , ("symbol?",    unaryOp "symbol?" symbolp)
    , ("integer?",   unaryOp "integer?" integerp)
    , ("rational?",  unaryOp "rational?" rationalp)
    , ("real?",      unaryOp "real?" realp)
    , ("complex?",   unaryOp "complex?" complexp)
    , ("number?",    unaryOp "number?" numberp)
    , ("character?", unaryOp "character?" characterp)
    , ("string?",    unaryOp "string?" stringp)
    , ("boolean?",   unaryOp "boolean?" booleanp)
    , ("list?",      unaryOp "list?" listp)
    , ("pair?",      unaryOp "pair?" pairp)
    -- Type converting functions
    , ("symbol->string", unaryOp "symbol->string" symbolToString)
    , ("string->symbol", unaryOp "string->symbol" stringToSymbol)
    ]

-- | Construct a lisp function with a binary numberical operator.
numericBinop
    :: String
    -> (Integer -> Integer -> Integer)
    -> [LispVal]
    -> ThrowsError LispVal
numericBinop fName _ []    = throwError $ NumArgs fName 2 []
numericBinop fName _ v@[_] = throwError $ NumArgs fName 2 v
numericBinop _ op params   = Integer . foldl1 op <$> mapM unpackNum params

-- | Construct a lisp function with one argument.
unaryOp :: String -> (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp _ f [v]   = return $ f v
unaryOp fName _ v = throwError $ NumArgs fName 1 v

-- | Type testing function `symbol?`.
symbolp :: LispVal -> LispVal
symbolp (Symbol _) = Boolean True
symbolp _          = Boolean False

-- | Type testing function `integer?`.
integerp :: LispVal -> LispVal
integerp (Integer _) = Boolean True
integerp _           = Boolean False

-- | Type testing function `rational?`.
rationalp :: LispVal -> LispVal
rationalp (Integer _)  = Boolean True
rationalp (Rational _) = Boolean True
rationalp _            = Boolean False

-- | Type testing function `real?`.
realp :: LispVal -> LispVal
realp (Integer _)  = Boolean True
realp (Rational _) = Boolean True
realp (Real _)     = Boolean True
realp _            = Boolean False

-- | Type testing function `complex?`.
complexp :: LispVal -> LispVal
complexp (Integer _)  = Boolean True
complexp (Rational _) = Boolean True
complexp (Real _)     = Boolean True
complexp (Complex _)  = Boolean True
complexp _            = Boolean False

-- | Type testing function `number?`.
numberp :: LispVal -> LispVal
numberp = complexp

-- | Type testing function `character?`.
characterp :: LispVal -> LispVal
characterp (Character _) = Boolean True
characterp _             = Boolean False

-- | Type testing function `string?`.
stringp :: LispVal -> LispVal
stringp (String _) = Boolean True
stringp _          = Boolean False

-- | Type testing function `boolean?`.
booleanp :: LispVal -> LispVal
booleanp (Boolean _) = Boolean True
booleanp _           = Boolean False

-- | Type testing function `list?`.
listp :: LispVal -> LispVal
listp (List _) = Boolean True
listp _        = Boolean False

-- | Type testing function `pair?`.
pairp :: LispVal -> LispVal
pairp (DottedList _ _) = Boolean True
pairp _                = Boolean False

-- | Convert a `Symbol` to `String`.
symbolToString :: LispVal -> LispVal
symbolToString (Symbol x) = String x
symbolToString _          = error "symbol->string: not a symbol"

-- | Convert a `String` to `Symbol`.
stringToSymbol :: LispVal -> LispVal
stringToSymbol (String x) = Symbol x
stringToSymbol _          = error "string->symbol: not a string"

-- | Unpack the integer value of the `LispVal`.
--
--   Current only `Integer` is supported. All other values will evaluate to `0`.
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Integer n) = return n
unpackNum v           = throwError $ TypeMismatch "number" v
