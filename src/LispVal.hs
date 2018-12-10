module LispVal
    (
      -- * Types
      LispVal(..)
    ) where

import Data.Array (Array, elems)
import Data.Complex (Complex(..), imagPart, realPart)
import Data.Ratio (numerator, denominator)

data LispVal
    = Identifier String
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
    deriving (Eq)

instance Show LispVal where
    show = showVal

-- | Show the s-expression for LispVal.
showVal :: LispVal -> String
showVal (Identifier x)    = x
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

-- | Join the s-expressions of a list of `LispVal` with spaces.
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
