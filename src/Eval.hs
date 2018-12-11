{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Eval
    (
      -- * Evaluate
      eval
    ) where

import LispVal (LispVal(..))

-- | Evaluate `LispVal` recursively.
eval :: LispVal -> LispVal
eval x@(Integer _)   = x
eval x@(Rational _)  = x
eval x@(Real _)      = x
eval x@(Complex _)   = x
eval x@(Character _) = x
eval x@(String _)    = x
eval x@(Boolean _)   = x
eval (List [Symbol "quote", x])   = x
eval (List (Symbol fName : args)) = apply fName $ map eval args

-- | Apply function by name and arguments.
apply :: String -> [LispVal] -> LispVal
apply fName args = maybe (Boolean False) ($ args) $ lookup fName primitives

-- | Primitive functions for lisp.
primitives :: [(String, [LispVal] -> LispVal)]
primitives =
    -- Numerical operators
    [ ("+",          numericBinop (+))
    , ("-",          numericBinop (-))
    , ("*",          numericBinop (*))
    , ("/",          numericBinop div)
    , ("mod",        numericBinop mod)
    , ("quotient",   numericBinop quot)
    , ("remainder",  numericBinop rem)
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
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Integer $ foldl1 op $ map unpackNum params

-- | Construct a lisp function with one argument.
unaryOp :: String -> (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp _ f [v]   = f v
unaryOp fName _ _ = error $ fName ++ ": wrong number of arguments"

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
unpackNum :: LispVal -> Integer
unpackNum (Integer n) = n
unpackNum _           = 0
