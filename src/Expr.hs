module Expr
    (
      -- * Expression
      readExpr
    ) where

import Data.Char (digitToInt)
import Numeric (readInt, readHex, readOct)

import Text.ParserCombinators.Parsec

data LispVal
    = Identifier String
    | Number Integer
    | Character Char
    | String String
    | Boolean Bool
    | List [LispVal]
    | DottedList [LispVal] LispVal
    deriving (Eq, Show)

-- | Read an expression. Returns the parsed `LispVal`.
readExpr :: String -> String
readExpr input =
    case parse parseExpr "lisp" input of
        Left err   -> "No match: " ++ show err
        Right expr -> show expr

-- | Parses an expression. Returns the parsed `LispVal`.
parseExpr :: Parser LispVal
parseExpr = parseIdentifier <|> parseNumber <|> parseString
    <|> try parseCharacter <|> parseBoolean <|> parseQuoted <|> do
        _ <- char '('
        x <- try parseList <|> parseDottedList
        _ <- char ')'
        return x

-- | Parses a boolean. Returns the parsed `Boolean`.
parseBoolean :: Parser LispVal
parseBoolean = do
    v <- try (string "#t") <|> string "#f"

    return $
        case v of
            "#t" -> Boolean True
            _    -> Boolean False

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

-- | Parses a list. Returns the parsed `List`.
parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces1

-- | Parses a dotted list. Returns the parsed `DottedList`.
parseDottedList :: Parser LispVal
parseDottedList = do
    h <- endBy parseExpr spaces1
    t <- char '.' >> spaces1 >> parseExpr
    return $ DottedList h t

-- | Parses a numberical constant. Returns the parsed `Number`.
parseNumber :: Parser LispVal
parseNumber =
    try parseDecimal <|> try parseBin <|> try parseOct <|> try parseHex

-- | Parses a decimal numberical constant. Returns the parsed `Number`.
parseDecimal :: Parser LispVal
parseDecimal = do
    _ <- optional $ string "#d"
    Number . read <$> many1 digit

-- | Parses an binary numberical constant. Returns the parsed `Number`.
parseBin :: Parser LispVal
parseBin = do
    _ <- string "#b"
    n <- many1 $ oneOf "01"
    return $
        case readBin n of
            [(v, "")] -> Number v
            _         -> error $ "parseBin: no match for " ++ n

-- | Parses an octal numberical constant. Returns the parsed `Number`.
parseOct :: Parser LispVal
parseOct = do
    _ <- string "#o"
    n <- many1 octDigit
    return $
        case readOct n of
            [(v, "")] -> Number v
            _         -> error $ "parseOct: no match for " ++ n

-- | Parses a hexadecimal numberics constant. Returns the parsed `Number`.
parseHex :: Parser LispVal
parseHex = do
    _ <- string "#x"
    n <- many1 hexDigit
    return $
        case readHex n of
            [(v, "")] -> Number v
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

parseQuotedCharacter :: Parser Char
parseQuotedCharacter = do
    _  <- char '\\'
    qc <- char '\\' <|> char '"' <|> char 'n' <|> char 'r' <|> char 't'
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
