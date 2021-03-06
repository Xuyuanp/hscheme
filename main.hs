{-# LANGUAGE ExistentialQuantification #-}
module Main where

import           Control.Monad
import           Control.Monad.Error
import           Data.IORef
import           Data.Maybe
import           System.Environment
import           System.IO
import           Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {
                    params  :: [String],
                    vararg  :: Maybe String,
                    body    :: [LispVal],
                    closure :: Env
                    }
            | IOFunc ([LispVal] -> IOThrowsError LispVal)
            | Port Handle

showVal :: LispVal -> String
showVal (String contents)        = "\"" ++ contents ++ "\""
showVal (Atom name)              = name
showVal (Number contents)        = show contents
showVal (Bool True)              = "#t"
showVal (Bool False)             = "#f"
showVal (List contents)          = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head1 tail1) = "(" ++ unwordsList head1 ++ " . " ++ showVal tail1 ++  ")"
showVal (PrimitiveFunc _)        = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = _, closure = _}) =
        "(lambda (" ++ unwords (map show args) ++
        (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _)                 = "<IO port>"
showVal (IOFunc _)               = "<IO primitive>"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (Default message)             = message

instance Show LispError where show = showError

instance Error LispError where
        noMsg = Default "An error has occurred"
        strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = liftM extractValue (runErrorT (trapError action))

isBound :: Env -> String -> IO Bool
isBound envRef var = liftM (isJust . lookup var) (readIORef envRef)

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
        env <- liftIO $ readIORef envRef
        maybe (throwError $ UnboundVar "Getting an unbound variable" var)
            (liftIO . readIORef)
            (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
        env <- liftIO $ readIORef envRef
        maybe (throwError $ UnboundVar "Setting an unbound variable" var)
            (liftIO . flip writeIORef value)
            (lookup var env)
        return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
        alreadyDefined <- liftIO $ isBound envRef var
        if alreadyDefined
            then setVar envRef var value >> return value
            else liftIO $ do
                valueRef <- newIORef value
                env <- readIORef envRef
                writeIORef envRef ((var, valueRef) : env)
                return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef where
    extendEnv bindings2 env = liftM (++ env) (mapM addBinding bindings2)
    addBinding (var, value) = do
        ref <- newIORef value
        return (var, ref)

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

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
                               Left err -> throwError $ Parser err
                               Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc                   = makeFunc Nothing
makeVarArgs                      = makeFunc . Just . showVal

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _)                                                               = return val
eval _ val@(Number _)                                                               = return val
eval _ val@(Bool _)                                                                 = return val
eval env (Atom a)                                                                   = getVar env a
eval _ (List [Atom "quote", val])                                                   = return val
eval env (List [Atom "if", preds, conseq, alt])                                     =
        do result <- eval env preds
           case result of
               Bool False -> eval env alt
               _ -> eval env conseq
eval env (List [Atom "set!", Atom var, form])                                       = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form])                                     = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params_) : body_))                 =
        makeNormalFunc env params_ body_ >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params_) varargs : body_))   =
        makeVarArgs varargs env params_ body_ >>= defineVar env var
eval env (List (Atom "lambda" : List params_ : body_))                              =
        makeNormalFunc env params_ body_
eval env (List (Atom "lambda" : DottedList params_ varargs : body_))                =
        makeVarArgs varargs env params_ body_
eval env (List (Atom "lambda" : varargs@(Atom _) : body_))                          =
        makeVarArgs varargs env [] body_
eval env (List [Atom "load", String filename])                                      =
        load filename >>= liftM last . mapM (eval env)
eval env (List (function : args))                                                   = do
        func <- eval env function
        argVals <- mapM (eval env) args
        apply func argVals
eval _ badForm                                                                      = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params_ varargs body_ closure_) args =
        if num params_ /= num args && isNothing varargs
            then throwError $ NumArgs (num params_) args
            else (liftIO $ bindVars closure_ $ zip params_ args) >>= bindVarArgs varargs >>= evalBody
        where
            remainingArgs = drop (length params_) args
            num = toInteger . length
            evalBody env = liftM last $ mapM (eval env) body_
            bindVarArgs arg env = case arg of
                                      Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
                                      Nothing -> return env
apply (IOFunc func) args = func args

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives) where
                            makeFunc constructor (var, func) = (var, constructor func)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+))
             ,("-", numericBinop (-))
             ,("*", numericBinop (*))
             ,("/", numericBinop div)
             ,("mod", numericBinop mod)
             ,("quotient", numericBinop quot)
             ,("remainder", numericBinop rem)
             ,("=", numBoolBinop (==))
             ,("<", numBoolBinop (<))
             ,(">", numBoolBinop (>))
             ,("/=", numBoolBinop (/=))
             ,(">=", numBoolBinop (>=))
             ,("<=", numBoolBinop (<=))
             ,("&&", boolBoolBinop (&&))
             ,("||", boolBoolBinop (||))
             ,("string=?", strBoolBinop (==))
             ,("string<?", strBoolBinop (<))
             ,("string>?", strBoolBinop (>))
             ,("string<=?", strBoolBinop (<=))
             ,("string>=?", strBoolBinop (>=))
             ,("car", car)
             ,("cdr", cdr)
             ,("cons", cons)
             ,("eq?", eqv)
             ,("eqv?", eqv)
             ,("equal?", equal)]

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                                 then throwError $ NumArgs 2 args
                                 else do
                                     left <- unpacker $ head args
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop  = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop  = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ []            = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params       = mapM unpackNum params >>= return . Number . foldl1 op

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)]         = return x
car [DottedList (x:_) _] = return x
car [badArg]              = throwError $ TypeMismatch "pair" badArg
car badArgList            = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg]              = throwError $ TypeMismatch "pair" badArg
cdr badArgList            = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]            = return $ List [x1]
cons [x, List xs]             = return $ List $ x:xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) &&
                                                             (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left _ -> False
                                Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
        unpacked1 <- unpacker arg1
        unpacked2 <- unpacker arg2
        return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProce)
               ,("open-input-file", makePort ReadMode)
               ,("open-output-file", makePort WriteMode)
               ,("close-input-port", closePort)
               ,("close-output-port", closePort)
               ,("read", readProc)
               ,("write", writeProc)
               ,("read-contents", readContents)
               ,("read-all", readAll)]

applyProce :: [LispVal] -> IOThrowsError LispVal
applyProce [func, List args] = apply func args
applyProce (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred1 prompt action = do
        result <- prompt
        unless (pred1 result) $ action result >> until_ pred1 prompt action

runOne :: [String] -> IO ()
runOne args = do
        env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
        (runIOThrows $ liftM show $ eval env (List [Atom "load", String (head args)])) >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

main :: IO ()
main = do
        args <- getArgs
        if null args then runRepl else runOne args
