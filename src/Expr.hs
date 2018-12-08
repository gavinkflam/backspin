module Expr
    (
      -- * Expression
      readExpr
    ) where

import Data.Array (Array, listArray)
import Data.Char (digitToInt)
import Data.Complex (Complex(..))
import Data.Ratio ((%))
import Numeric (readFloat, readHex, readInt, readOct)

import Text.ParserCombinators.Parsec

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
    deriving (Eq, Show)

-- | Read an expression. Returns the parsed `LispVal`.
readExpr :: String -> String
readExpr input =
    case parse parseExpr "lisp" input of
        Left err   -> "No match: " ++ show err
        Right expr -> show expr

-- | Parses an expression. Returns the parsed `LispVal`.
parseExpr :: Parser LispVal
parseExpr = parseIdentifier
    -- Expressions started with `#`
    <|> try parseNumber <|> try parseBoolean <|> try parseCharacter
    <|> parseVector
    -- Expressions started with `unquote` (,)
    <|> try parseUnquoteSplicing <|> parseUnquote
    <|> parseString <|> parseQuasiquote <|> parseQuoted
    <|> try parseList <|> parseDottedList

-- | Parses a boolean. Returns the parsed `Boolean`.
parseBoolean :: Parser LispVal
parseBoolean = do
    _ <- char '#'
    v <- char 't' <|> char 'f'

    return $
        case v of
            't' -> Boolean True
            _   -> Boolean False

-- | Parses an identifier. Returns the parsed `Identifier`.
parseIdentifier :: Parser LispVal
parseIdentifier = do
    first <- letter <|> extendedAlphabet
    rest  <- many $ letter <|> digit <|> extendedAlphabet
    return $ Identifier $ first:rest

-- | Parses a literal expression. Returns the parsed `List`.
parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ List [Identifier "quote", x]

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
    return $ List [Identifier "quasiquote", x]

-- | Parses an unquote expression. Returns the parsed `List`.
parseUnquote :: Parser LispVal
parseUnquote = do
    _ <- char ','
    x <- parseExpr
    return $ List [Identifier "unquote", x]

-- | Parses an unquote-splicing expression. Returns the parsed `List`.
parseUnquoteSplicing :: Parser LispVal
parseUnquoteSplicing = do
    _ <- string ",@"
    x <- parseExpr
    return $ List [Identifier "unquote-splicing", x]

-- | Parses a list. Returns the parsed `List`.
parseList :: Parser LispVal
parseList = do
    _  <- char '('
    xs <- sepBy parseExpr spaces1
    _  <- char ')'
    return $ List xs

-- | Parses a dotted list. Returns the parsed `DottedList`.
parseDottedList :: Parser LispVal
parseDottedList = do
    _  <- char '('
    xs <- endBy parseExpr spaces1
    t  <- char '.' >> spaces1 >> parseExpr
    _  <- char ')'
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
