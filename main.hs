module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad --For liftM

-- Data type for our values
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

-- Defining a symbol parser
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- Defining a whitespace parser
spaces :: Parser ()
spaces = skipMany1 space

-- Defining a String (LispVal) Parser
parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x

-- Defining an Atom parser
parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

-- Defining a Number parser
parseNumber :: Parser LispVal
parseNumber = many1 digit >>= (return . Number . read)

-- Defining a separate exp parser
parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber

-- Parsing and treating errors
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main = do
        args <- getArgs
        putStrLn (readExpr (args !! 0))
