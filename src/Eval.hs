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
eval (List [Identifier "quote", x])   = x
eval (List (Identifier fName : args)) = apply fName $ map eval args

-- | Apply function by name and arguments.
apply :: String -> [LispVal] -> LispVal
apply fName args = maybe (Boolean False) ($ args) $ lookup fName primitives

-- | Primitive functions for lisp.
primitives :: [(String, [LispVal] -> LispVal)]
primitives =
    [ ("+",         numericBinop (+))
    , ("-",         numericBinop (-))
    , ("*",         numericBinop (*))
    , ("/",         numericBinop div)
    , ("mod",       numericBinop mod)
    , ("quotient",  numericBinop quot)
    , ("remainder", numericBinop rem)
    ]

-- | Construct a lisp function with a binary numberical operator.
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Integer $ foldl1 op $ map unpackNum params

-- | Unpack the integer value of the `LispVal`.
--
--   Current only `Integer` is supported. All other values will evaluate to `0`.
unpackNum :: LispVal -> Integer
unpackNum (Integer n) = n
unpackNum _           = 0
