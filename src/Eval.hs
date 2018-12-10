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
    , ("symbol?",    typeSymbolPredicate)
    , ("integer?",   typeIntegerPredicate)
    , ("rational?",  typeRationalPredicate)
    , ("real?",      typeRealPredicate)
    , ("complex?",   typeComplexPredicate)
    , ("number?",    typeNumberPredicate)
    , ("character?", typeCharacterPredicate)
    , ("string?",    typeStringPredicate)
    , ("boolean?",   typeBooleanPredicate)
    , ("list?",      typeListPredicate)
    , ("pair?",      typePairPredicate)
    ]

-- | Construct a lisp function with a binary numberical operator.
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Integer $ foldl1 op $ map unpackNum params

-- | Type testing function `symbol?`.
typeSymbolPredicate :: [LispVal] -> LispVal
typeSymbolPredicate [Symbol _] = Boolean True
typeSymbolPredicate [_]        = Boolean False
typeSymbolPredicate _          = error "symbol?: wrong number of arguments"

-- | Type testing function `integer?`.
typeIntegerPredicate :: [LispVal] -> LispVal
typeIntegerPredicate [Integer _] = Boolean True
typeIntegerPredicate [_]         = Boolean False
typeIntegerPredicate _           = error "integer?: wrong number of arguments"

-- | Type testing function `rational?`.
typeRationalPredicate :: [LispVal] -> LispVal
typeRationalPredicate [Integer _]  = Boolean True
typeRationalPredicate [Rational _] = Boolean True
typeRationalPredicate [_]          = Boolean False
typeRationalPredicate _            = error "rational?: wrong number of arguments"

-- | Type testing function `real?`.
typeRealPredicate :: [LispVal] -> LispVal
typeRealPredicate [Integer _]  = Boolean True
typeRealPredicate [Rational _] = Boolean True
typeRealPredicate [Real _]     = Boolean True
typeRealPredicate [_]          = Boolean False
typeRealPredicate _            = error "real?: wrong number of arguments"

-- | Type testing function `complex?`.
typeComplexPredicate :: [LispVal] -> LispVal
typeComplexPredicate [Integer _]  = Boolean True
typeComplexPredicate [Rational _] = Boolean True
typeComplexPredicate [Real _]     = Boolean True
typeComplexPredicate [Complex _]  = Boolean True
typeComplexPredicate [_]          = Boolean False
typeComplexPredicate _            = error "complex?: wrong number of arguments"

-- | Type testing function `number?`.
typeNumberPredicate :: [LispVal] -> LispVal
typeNumberPredicate v@[_] = typeComplexPredicate v
typeNumberPredicate _     = error "number?: wrong number of arguments"

-- | Type testing function `character?`.
typeCharacterPredicate :: [LispVal] -> LispVal
typeCharacterPredicate [Character _] = Boolean True
typeCharacterPredicate [_]           = Boolean False
typeCharacterPredicate _             = error "character?: wrong number of arguments"

-- | Type testing function `string?`.
typeStringPredicate :: [LispVal] -> LispVal
typeStringPredicate [String _] = Boolean True
typeStringPredicate [_]        = Boolean False
typeStringPredicate _          = error "string?: wrong number of arguments"

-- | Type testing function `boolean?`.
typeBooleanPredicate :: [LispVal] -> LispVal
typeBooleanPredicate [Boolean _] = Boolean True
typeBooleanPredicate [_]         = Boolean False
typeBooleanPredicate _           = error "boolean?: wrong number of arguments"

-- | Type testing function `list?`.
typeListPredicate :: [LispVal] -> LispVal
typeListPredicate [List _] = Boolean True
typeListPredicate [_]      = Boolean False
typeListPredicate _        = error "list?: wrong number of arguments"

-- | Type testing function `pair?`.
typePairPredicate :: [LispVal] -> LispVal
typePairPredicate [DottedList _ _] = Boolean True
typePairPredicate [_]              = Boolean False
typePairPredicate _                = error "pair?: wrong number of arguments"

-- | Unpack the integer value of the `LispVal`.
--
--   Current only `Integer` is supported. All other values will evaluate to `0`.
unpackNum :: LispVal -> Integer
unpackNum (Integer n) = n
unpackNum _           = 0
