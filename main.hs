module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
                     Left err -> "No match: " ++ show err
                     Right _ -> "Found value"

main :: IO ()
main = do
        (expr:_) <- getArgs
        putStrLn $ readExpr expr
