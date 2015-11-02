module Main where

import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
        _ <- char '"'
        x <- many (noneOf "\"")
        _ <- char '"'
        return $ String x

parseAtom :: Parser LispVal
parseAtom = do
        first <- letter <|> symbol
        rest <- many (letter <|> digit <|> symbol)
        let atom = first:rest
        return $ case atom of
                     "#t" -> Bool True
                     "#f" -> Bool False
                     _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
        head1 <- endBy parseExpr spaces
        tail1 <- char '.' >> spaces >> parseExpr
        return $ DottedList head1 tail1

parseQuoted :: Parser LispVal
parseQuoted = do
        _ <- char '\''
        x <- parseExpr
        return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do
            _ <- char '('
            x <- try parseList <|> parseDottedList
            _ <- char ')'
            return x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                     Left err -> "No match: " ++ show err
                     Right _ -> "Found value"

main :: IO ()
main = do
        (expr:_) <- getArgs
        putStrLn $ readExpr expr
