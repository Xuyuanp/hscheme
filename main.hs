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
eval (List (Atom func : args))  = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in 
                           if null parsed 
                              then 0
                              else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
