module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.Ratio
import Data.Complex
import Control.Monad --For liftM
import Numeric

-- Data type for our values
data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Number Integer
            | String String
            | Bool Bool
            | Character Char
            | Float Double
            | Ratio Rational
            | Complex (Complex Double)

-- Defining a symbol parser
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

-- Defining a Character parser
parseCharacter :: Parser LispVal
parseCharacter = do
    try $ string "#\\"
    value <- try (string "newline" <|> string "space") 
            <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
    return $ Character $ case value of
            "space" -> ' '
            "newline" -> '\n'
            otherwise -> (value !! 0)

-- Defining an escaped char parser
escapedChars :: Parser Char
escapedChars = do 
                char '\\' -- a backslash
                x <- oneOf "\\\"nrt" -- either backslash or doublequote
                return $ case x of
                    '\\' -> x
                    '"'  -> x
                    'n'  -> '\n'
                    'r'  -> '\r'
                    't'  -> '\t'

--Defining a whitespace parser
spaces :: Parser ()
spaces = skipMany1 space

-- Defining a String (LispVal) Parser
parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many $ escapedChars <|> noneOf "\"\\"
                char '"'
                return $ String x

parseBool :: Parser LispVal
parseBool = do
                char '#'
                (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

-- Defining an Atom parser
parseAtom :: Parser LispVal
parseAtom = do
                first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = first:rest
                return $ Atom atom

-- Defining a Number parser
parseNumber :: Parser LispVal
parseNumber = parseDigital1
                <|> parseDigital2
                <|> parseHex
                <|> parseOct
                <|> parseBin

-- Defining a Float parser
parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Float (fst.head$readFloat (x++"."++y))

-- Defining a Ratio Parser
parseRatio :: Parser LispVal
parseRatio = do x <- many1 digit
                char '/'
                y <- many1 digit
                return $ Ratio ((read x) % (read y))

-- Double converter helper function
toDouble :: LispVal -> Double
toDouble(Float f) = f
toDouble(Number n) = fromIntegral n

parseComplex :: Parser LispVal
parseComplex = do 
                x <- try parseFloat
                char '+' 
                y <- try parseFloat
                char 'i' 
                return $ Complex (toDouble x :+ toDouble y)

-- Old style parser
parseDigital1 :: Parser LispVal
parseDigital1 = many1 digit >>= (return . Number . read)

-- New style parser
parseDigital2 :: Parser LispVal
parseDigital2 = do 
                    try $ string "#d"
                    x <- many1 digit
                    (return . Number . read) x

-- Hex parser
parseHex :: Parser LispVal
parseHex = do 
            try $ string "#x"
            x <- many1 hexDigit
            return $ Number (hex2dig x)

-- Oct parser
parseOct :: Parser LispVal
parseOct = do 
            try $ string "#o"
            x <- many1 octDigit
            return $ Number (oct2dig x)

-- Bin parser
parseBin :: Parser LispVal
parseBin = do 
            try $ string "#b"
            x <- many1 (oneOf "10")
            return $ Number (bin2dig x)

-- Helper functions
oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig  = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                            bin2dig' old xs

-- Defining a separate exp parser
parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseFloat
        <|> try parseRatio
        <|> try parseFloat
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter

-- Parsing and treating errors
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main = do
        args <- getArgs
        putStrLn (readExpr (args !! 0))
