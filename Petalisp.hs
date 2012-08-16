module Main (main) where
import System.Environment
import System.Console.Readline
import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_-"

spaces :: Parser()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundedVar String String
               | Default String

showError :: LispError -> String
showError (NumArgs expected found) = "Expeted " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ show expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundedVar message varname) = message ++ ": " ++ varname
showError (Default message) = show message

instance Show LispError where show = showError

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

instance Show LispVal where show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op 

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                          if null parsed
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many ( parseEscaped <|> noneOf "\"" )
    char '"'
    return $ String x

parseEscaped = do
    char '\\'
    c <- anyChar
    case c of
        '\\' -> return '\\'
        'n' -> return '\n'
        'r' -> return '\r'
        't' -> return '\t'
        '"' -> return '"'

parseAtom :: Parser LispVal
parseAtom = do
          first <- letter <|> symbol
          rest <- many (letter <|> digit <|> symbol)
          let atom = first:rest
          return $ case atom of
                     "#t" -> Bool True
                     "#f" -> Bool False
                     _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

main :: IO ()
main = repl

repl :: IO ()
repl = do
  maybeLine <- readline "% "
  case maybeLine of
    Nothing -> return ()
    Just "exit" -> return ()
    Just "quit" -> return ()
    Just "" -> repl
    Just line -> do addHistory line
                    evaled <- return $ liftM show $ readExpr line >>= eval
                    putStrLn $ extractValue $ trapError evaled
                    repl