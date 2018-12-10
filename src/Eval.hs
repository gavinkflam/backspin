{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Eval
    (
      -- * Evaluate
      eval
    ) where

import LispVal (LispVal(..))

eval :: LispVal -> LispVal
eval x@(Integer _)   = x
eval x@(Rational _)  = x
eval x@(Real _)      = x
eval x@(Complex _)   = x
eval x@(Character _) = x
eval x@(String _)    = x
eval x@(Boolean _)   = x
eval (List [Identifier "quote", x]) = x
