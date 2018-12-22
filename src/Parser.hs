module Parser
    (
      -- * Expression
      readExpr
    ) where

import Data.Array (listArray)
import Data.Char (digitToInt)
import Data.Complex (Complex(..))
import Data.Ratio ((%))
import Numeric (readFloat, readHex, readInt, readOct)

import Control.Monad.Except (throwError)
import Text.ParserCombinators.Parsec

import Type (LispError(..), LispVal(..), ThrowsError)

-- | Read an expression. Returns the parsed `LispVal`.
readExpr :: String -> ThrowsError LispVal
readExpr input =
    case parse parseExpr "lisp" input of
        Left err   -> throwError $ Parser err
        Right expr -> return expr

-- | Parses an expression. Returns the parsed `LispVal`.
parseExpr :: Parser LispVal
parseExpr = parseSymbol
    -- Expressions started with `#`
    <|> try parseNumber <|> try parseBoolean <|> try parseCharacter
    <|> parseVector
    -- Expressions started with `unquote` (,)
    <|> try parseUnquoteSplicing <|> parseUnquote
    <|> parseString <|> parseQuasiquote <|> parseQuoted <|> parseList

-- | Parses a boolean. Returns the parsed `Boolean`.
parseBoolean :: Parser LispVal
parseBoolean = do
    _ <- char '#'
    v <- char 't' <|> char 'f'

    return $
        case v of
            't' -> Boolean True
            _   -> Boolean False

-- | Parses an symbol. Returns the parsed `Symbol`.
parseSymbol :: Parser LispVal
parseSymbol = do
    first <- letter <|> extendedAlphabet
    rest  <- many $ letter <|> digit <|> extendedAlphabet
    return $ Symbol $ first:rest

-- | Parses a literal expression. Returns the parsed `List`.
parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ List [Symbol "quote", x]

-- | Parses a vector. Returns the parsed `Vector`.
parseVector :: Parser LispVal
parseVector = do
    _  <- string "#("
    xs <- sepBy parseExpr spaces1
    _  <- char ')'
    return $ Vector $ listArray (0, length xs - 1) xs

-- | Parses a quasiquote expression. Returns the parsed `List`.
parseQuasiquote :: Parser LispVal
parseQuasiquote = do
    _ <- char '`'
    x <- parseExpr
    return $ List [Symbol "quasiquote", x]

-- | Parses an unquote expression. Returns the parsed `List`.
parseUnquote :: Parser LispVal
parseUnquote = do
    _ <- char ','
    x <- parseExpr
    return $ List [Symbol "unquote", x]

-- | Parses an unquote-splicing expression. Returns the parsed `List`.
parseUnquoteSplicing :: Parser LispVal
parseUnquoteSplicing = do
    _ <- string ",@"
    x <- parseExpr
    return $ List [Symbol "unquote-splicing", x]

-- | Parses a list. Returns the parsed `List` or `DottedList`.
parseList :: Parser LispVal
parseList = do
    _  <- char '(' >> spaces
    xs <- parseExpr `sepEndBy` spaces1
    parseDottedListTail xs <|> parseListEnd xs

-- | Parses the end of a list. Returns the parsed `List`.
parseListEnd :: [LispVal] -> Parser LispVal
parseListEnd xs = do
    _  <- spaces >> char ')'
    return $ List xs

-- | Parses the tail of a dotted list. Returns the parsed `DottedList`.
parseDottedListTail :: [LispVal] -> Parser LispVal
parseDottedListTail xs = do
    _  <- char '.' >> spaces1
    t  <- parseExpr
    _  <- spaces >> char ')'
    return $ DottedList xs t

-- | Parses a numberical constant.
--
--   Returns the parsed `Integer`, `Rational`, `Real` or `Complex`.
parseNumber :: Parser LispVal
parseNumber =
    try parseReal <|> try parseRational <|> try parseComplex
    <|> try parseBin <|> try parseOct <|> try parseHex <|> parseDecimal

-- | Parses a floating point constant. Returns the parsed `Real`.
parseReal :: Parser LispVal
parseReal = do
    i <- many1 digit
    _ <- char '.'
    d <- many1 digit

    let n = concat [i, ".", d]
    return $
        case readFloat n of
            [(v, "")] -> Real v
            _         -> error $ "parseReal: no match for " ++ n

-- | Parses an rational number constant. Returns the parsed `Rational`.
--
--   For `a/b`, only integers are supported for `a` and `b` to ease
--   implementation.
parseRational :: Parser LispVal
parseRational = do
    n <- many1 digit
    _ <- char '/'
    d <- many1 digit
    return $ Rational $ read n % read d

-- | Parses an complex number constant. Returns the parsed `Complex`.
--
--   For `a+bi`, only integers are supported for `a` and `b` to ease
--   implementation.
parseComplex :: Parser LispVal
parseComplex = do
    n <- many1 digit
    _ <- char '+'
    d <- many1 digit
    _ <- char 'i'
    return $ Complex $ read n :+ read d

-- | Parses a decimal numberical constant. Returns the parsed `Integer`.
parseDecimal :: Parser LispVal
parseDecimal = do
    _ <- optional $ string "#d"
    Integer . read <$> many1 digit

-- | Parses an binary numberical constant. Returns the parsed `Integer`.
parseBin :: Parser LispVal
parseBin = do
    _ <- string "#b"
    n <- many1 $ oneOf "01"
    return $
        case readBin n of
            [(v, "")] -> Integer v
            _         -> error $ "parseBin: no match for " ++ n

-- | Parses an octal numberical constant. Returns the parsed `Integer`.
parseOct :: Parser LispVal
parseOct = do
    _ <- string "#o"
    n <- many1 octDigit
    return $
        case readOct n of
            [(v, "")] -> Integer v
            _         -> error $ "parseOct: no match for " ++ n

-- | Parses a hexadecimal numberics constant. Returns the parsed `Integer`.
parseHex :: Parser LispVal
parseHex = do
    _ <- string "#x"
    n <- many1 hexDigit
    return $
        case readHex n of
            [(v, "")] -> Integer v
            _         -> error $ "parseHex: no match for " ++ n

-- | Parses a character constant. Returns the parsed `Character`.
--
--   Only `#\space` is supported instead of `#\` to ease implementation.
parseCharacter :: Parser LispVal
parseCharacter = do
    _ <- string "#\\"
    s <- string "space" <|> string "newline" <|> count 1 (noneOf " ")
    return $
        case s of
            "space"   -> Character ' '
            "newline" -> Character '\n'
            [c]       -> Character c
            _         -> error $ "parseCharacter: no match for " ++ s

-- | Parses a string constant. Returns the parsed `String`.
parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many $ noneOf "\\\"" <|> parseQuotedCharacter
    _ <- char '"'
    return $ String x

-- | Parses a quoted character (\\, \", \n, \r, \t).
--
--   Returns the parsed character.
parseQuotedCharacter :: Parser Char
parseQuotedCharacter = do
    _  <- char '\\'
    qc <- oneOf "\\\"nrt"
    return $
        case qc of
            'n' -> '\n'
            'r' -> '\r'
            't' -> '\t'
            _   -> qc

-- | Skips one or more white space characters.
spaces1 :: Parser ()
spaces1 = skipMany1 space

-- | Parses an extended alphabetic character (!$%&*+-/:<=>?@^_~).
--
--   Dot (.) was removed to ease implementation of dotted list.
--
--   Returns the parsed character.
extendedAlphabet :: Parser Char
extendedAlphabet = oneOf "!$%&*+-/:<=>?@^_~"

-- | Read an unsigned number in binary notation.
readBin :: (Eq a, Num a) => ReadS a
readBin = readInt 2 (`elem` "01") digitToInt
