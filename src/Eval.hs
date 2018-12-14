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
    (throwError $ NotFunction "Unrecognized primitive function" fName)
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
    -- Numerical binary operators
    , ("=",          numBoolBinop "=" (==))
    , ("<",          numBoolBinop "<" (<))
    , (">",          numBoolBinop ">" (>))
    , ("/=",         numBoolBinop "/=" (/=))
    , (">=",         numBoolBinop ">=" (>=))
    , ("<=",         numBoolBinop "<=" (<=))
    -- Boolean binary operators
    , ("and",        boolBoolBinop "and" (&&))
    , ("or",         boolBoolBinop "or" (||))
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

-- Construct a binary operator which the results can be cumulated along the
-- variable length arguments.
cumulativeBinop
    :: (String -> LispVal -> ThrowsError a)
    -> (a -> LispVal)
    -> String
    -> (a -> a -> a)
    -> [LispVal]
    -> ThrowsError LispVal
cumulativeBinop _ _ name _ []    = throwError $ NumArgs name 2 []
cumulativeBinop _ _ name _ v@[_] = throwError $ NumArgs name 2 v
cumulativeBinop unpack constructor name op pms =
    constructor . foldl1 op <$> mapM (unpack name) pms

-- Construct a binary operator which the results can be chained with a
-- concatenating function along the variable length arguments.
chainableBinop
    :: (String -> LispVal -> ThrowsError a)
    -> (b -> LispVal)
    -> (b -> b -> b)
    -> b
    -> String
    -> (a -> a -> b)
    -> [LispVal]
    -> ThrowsError LispVal
chainableBinop _ _ _ _ name _ []    = throwError $ NumArgs name 2 []
chainableBinop _ _ _ _ name _ v@[_] = throwError $ NumArgs name 2 v
chainableBinop unpack constructor fConcat start name op pms  =
    constructor . snd . uncurry (foldl f) . g <$> mapM (unpack name) pms
  where
    f (n, b) x = (x, b `fConcat` (n `op` x))
    g (y:ys)   = ((y, start), ys)
    g _        = error $ name ++ ": arguments error, this shloud not happen"

-- | Construct a lisp function with a binary numberical operator.
numericBinop
    :: String
    -> (Integer -> Integer -> Integer)
    -> [LispVal]
    -> ThrowsError LispVal
numericBinop = cumulativeBinop unpackNum Integer

-- | Construct a lisp function with a binary numberical predicate function.
numBoolBinop
    :: String
    -> (Integer -> Integer -> Bool)
    -> [LispVal]
    -> ThrowsError LispVal
numBoolBinop = chainableBinop unpackNum Boolean (&&) True

-- | Construct a lisp function with a binary boolean predicate function.
boolBoolBinop
    :: String -> (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = cumulativeBinop unpackBool Boolean

-- | Construct a lisp function with one argument.
unaryOp
    :: String
    -> (LispVal -> ThrowsError LispVal)
    -> [LispVal]
    -> ThrowsError LispVal
unaryOp _ f [v]   = f v
unaryOp fName _ v = throwError $ NumArgs fName 1 v

-- | Type testing function `symbol?`.
symbolp :: LispVal -> ThrowsError LispVal
symbolp (Symbol _) = return $ Boolean True
symbolp _          = return $ Boolean False

-- | Type testing function `integer?`.
integerp :: LispVal -> ThrowsError LispVal
integerp (Integer _) = return $ Boolean True
integerp _           = return $ Boolean False

-- | Type testing function `rational?`.
rationalp :: LispVal -> ThrowsError LispVal
rationalp (Integer _)  = return $ Boolean True
rationalp (Rational _) = return $ Boolean True
rationalp _            = return $ Boolean False

-- | Type testing function `real?`.
realp :: LispVal -> ThrowsError LispVal
realp (Integer _)  = return $ Boolean True
realp (Rational _) = return $ Boolean True
realp (Real _)     = return $ Boolean True
realp _            = return $ Boolean False

-- | Type testing function `complex?`.
complexp :: LispVal -> ThrowsError LispVal
complexp (Integer _)  = return $ Boolean True
complexp (Rational _) = return $ Boolean True
complexp (Real _)     = return $ Boolean True
complexp (Complex _)  = return $ Boolean True
complexp _            = return $ Boolean False

-- | Type testing function `number?`.
numberp :: LispVal -> ThrowsError LispVal
numberp = complexp

-- | Type testing function `character?`.
characterp :: LispVal -> ThrowsError LispVal
characterp (Character _) = return $ Boolean True
characterp _             = return $ Boolean False

-- | Type testing function `string?`.
stringp :: LispVal -> ThrowsError LispVal
stringp (String _) = return $ Boolean True
stringp _          = return $ Boolean False

-- | Type testing function `boolean?`.
booleanp :: LispVal -> ThrowsError LispVal
booleanp (Boolean _) = return $ Boolean True
booleanp _           = return $ Boolean False

-- | Type testing function `list?`.
listp :: LispVal -> ThrowsError LispVal
listp (List _) = return $ Boolean True
listp _        = return $ Boolean False

-- | Type testing function `pair?`.
pairp :: LispVal -> ThrowsError LispVal
pairp (DottedList _ _) = return $ Boolean True
pairp _                = return $ Boolean False

-- | Convert a `Symbol` to `String`.
symbolToString :: LispVal -> ThrowsError LispVal
symbolToString (Symbol x) = return $ String x
symbolToString v =
    throwError $ TypeMismatch "symbol->string" "symbol" v

-- | Convert a `String` to `Symbol`.
stringToSymbol :: LispVal -> ThrowsError LispVal
stringToSymbol (String x) = return $ Symbol x
stringToSymbol v =
    throwError $ TypeMismatch "string->symbol" "symbol" v

-- | Unpack the integer value of the `LispVal`.
--
--   Current only `Integer` is supported. All other values will evaluate to `0`.
unpackNum :: String -> LispVal -> ThrowsError Integer
unpackNum _ (Integer n) = return n
unpackNum fName v       = throwError $ TypeMismatch fName "number" v

-- | Unpack the boolean value of the `Boolean`.
unpackBool :: String -> LispVal -> ThrowsError Bool
unpackBool _ (Boolean n) = return n
unpackBool fName v       = throwError $ TypeMismatch fName "boolean" v
