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

showVal :: LispVal -> String
showVal (String contents)        = "\"" ++ contents ++ "\""
showVal (Atom name)              = name
showVal (Number contents)        = show contents
showVal (Bool True)              = "#t"
showVal (Bool False)             = "#f"
showVal (List contents)          = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head1 tail1) = "(" ++ unwordsList head1 ++ " . " ++ showVal tail1 ++  ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

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

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
                     Left err -> String $ "No match: " ++ show err
                     Right val -> val

eval :: LispVal -> LispVal
eval val@(String _)             = val
eval val@(Number _)             = val
eval val@(Bool _)               = val
eval (List [Atom "quote", val]) = val

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
