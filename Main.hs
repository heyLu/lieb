{-# LANGUAGE PackageImports #-}
import Prelude hiding (showList)
import Numeric
import Data.Ratio
import Control.Monad (liftM)
import System.Environment (getArgs)
import "mtl" Control.Monad.Error

import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | Float Float
    | String String
    | Char Char
    | Bool Bool

instance Show LispVal where
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (Number n) = show n
    show (Float f) = show f
    show (String s) = show s
    show (Atom a) = a
    show (Char c) = "#\\" ++ show c
    show (List l) = "(" ++ showList l ++ ")"
    show (DottedList head tail) = "(" ++ showList head ++ "." ++ show tail ++ ")"

showList = unwords . map show

instance Eq LispVal where
    (Atom x) == (Atom y) = x == y
    (Number x) == (Number y) = x == y
    (Float x) == (Float y) = x == y
    (String x) == (String y) = x == y
    (Char x) == (Char y) = x == y
    (Bool x) == (Bool y) = x == y
    (List xs) == (List ys) = xs `eqList` ys
    (DottedList xs xt) == (DottedList ys yt) =
        xs `eqList` ys && xt == yt
    _ == _ = False

eqList :: (Eq a) => [a] -> [a] -> Bool
eqList xs ys = length xs == length ys
    && all (\(x, y) -> x == y) (zip xs ys)

instance Ord LispVal where
    (Atom x) `compare` (Atom y) = x `compare` y
    (Number x) `compare` (Number y) = x `compare` y
    (Float x) `compare` (Float y) = x `compare` y
    (String x) `compare` (String y) = x `compare` y
    (Char x) `compare` (Char y) = x `compare` y
    (Bool x) `compare` (Bool y) = x `compare` y
    _ `compare` _ = error "Can't compare"


data LispError = NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | Default String

instance Show LispError where
    show (NumArgs expected found) = "Expected " ++ show expected
        ++ " args; found values " ++ (unwords $ map show found)
    show (TypeMismatch expected found) = "Invalid type: Exptected " ++ expected
        ++ "; found " ++ show found
    show (Parser err) = "Parse error at " ++ show err
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunction message func) = message ++ ": " ++ show func
    show (UnboundVar message varname) = message ++ ": " ++ show varname

instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseChar :: Parser LispVal
parseChar = do
    string "#\\"
    liftM Char $ parseCharacterName <|> anyChar

parseCharacterName :: Parser Char
parseCharacterName = do
    characterName <- string "space" <|> string "newline" <|> string "tab"
    return $ case characterName of
        "space" -> ' '
        "newline" -> '\n'
        "tab" -> '\t'

escaped :: Parser Char
escaped = do
    char '\\'
    c <- oneOf "nrt\\\""
    return $ case c of
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        '\\' -> '\\'
        '\"' -> '\"'

parseString :: Parser LispVal
parseString = do
    char '"'
    str <- many (escaped <|> noneOf "\"")
    char '"'
    return $ String str

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseWithRead :: ReadS a -> String -> Parser a
parseWithRead r s = case r s of
    [(x, _)] -> return x
    _ -> fail "oops"

readBin :: (Integral a) => ReadS a
readBin = readInt 2 (`elem` "01") (\c -> case c of '0' -> 0; '1' -> 1)

parsePrefixedNumber :: Parser LispVal
parsePrefixedNumber = do
    char '#'
    radix <- oneOf "bodx"
    num <- case radix of
        'b' -> many1 (oneOf "01") >>= parseWithRead readBin
        'o' -> many1 (oneOf "01234567") >>= parseWithRead readOct
        'd' -> many1 digit >>= parseWithRead readDec
        'x' -> many1 (digit <|> oneOf "abcdef") >>= parseWithRead readHex
    return $ Number num

parseFloat :: Parser LispVal
parseFloat = do
    num <- liftM concat $ sequence [many1 digit, string ".", many1 digit]
    liftM Float $ parseWithRead readFloat num

parseNumber :: Parser LispVal
parseNumber = try parseFloat
    <|> (many1 digit >>= liftM Number . parseWithRead readDec)
    <|> parsePrefixedNumber

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

parseUnquoted :: Parser LispVal
parseUnquoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "unquote", x]

parseExpr :: Parser LispVal
parseExpr = try parseChar
    <|> try parseNumber
    <|> parseQuoted <|> parseUnquoted
    <|> parseAtom
    <|> parseString
    <|> do char '('
           x <- try parseList <|> parseDottedList
           char ')'
           return x

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lieb" input of
    Left err -> throwError $ Parser err
    Right val -> return val

eval :: LispVal -> ThrowsError LispVal
eval val@(Char _) = return val
eval val@(Bool _) = return val
eval val@(String _) = return val
eval val@(Float _) = return val
eval val@(Number _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = do
    result <- eval pred
    case result of
        Bool False -> eval alt
        otherwise  -> eval conseq
eval (List (Atom fn : args)) = mapM eval args >>= apply fn
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply fn args = maybe (throwError $ NotFunction "Unrecognized primitive function args" fn)
                      ($ args)
                      (lookup fn primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericFn (+) 0),
    ("-", numericFn' (-) negate),
    ("*", numericFn (*) 1),
    ("/", division),
    ("mod", numericBinOp mod),
    ("quotient", numericBinOp quot),
    ("remainder", numericBinOp rem),
    ("=", boolBinOp (==)),
    ("/=", boolBinOp (/=)),
    ("<", boolBinOp (<)),
    (">", boolBinOp (>)),
    ("<=", boolBinOp (<=)),
    (">=", boolBinOp (>=)),
    ("and", binOp unpackBool Bool (&&)),
    ("or", binOp unpackBool Bool (||))]

numericFn :: (Integer -> Integer -> Integer) -> Integer -> [LispVal] -> ThrowsError LispVal
numericFn fn defaultVal [] = return $ Number defaultVal
numericFn fn _ args = numericBinOp fn args

numericFn' :: (Integer -> Integer -> Integer) -> (Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericFn' _ oneArgFn [val] = unpackNumber val >>= return . Number . oneArgFn
numericFn' fn _ args = numericBinOp fn args

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp _ [] = throwError $ NumArgs 2 []
numericBinOp _ oneArg@[_] = throwError $ NumArgs 2 oneArg
numericBinOp fn args = mapM unpackNumber args >>= return . Number . foldl1 fn

unpackNumber :: LispVal -> ThrowsError Integer
unpackNumber (Number n) = return n
unpackNumber val = throwError $ TypeMismatch "number" val

division :: [LispVal] -> ThrowsError LispVal
division [(Number n)] = return . Float $ 1 / (fromInteger n)
division [(Float f)] = return . Float $ 1 / f
division args = foldM divide (head args) args

divide :: LispVal -> LispVal -> ThrowsError LispVal
divide x y = do
    x' <- unpack x
    y' <- unpack y
    let q = toRational $ x' / y'
    return $ if denominator q == 1
             then Number . numerator $ q
             else Float $ fromRational q
  where unpack (Number n) = return $ fromInteger n
        unpack (Float f) = return f
        unpack val = throwError $ TypeMismatch "number" val

boolBinOp :: (LispVal -> LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinOp fn [x, y] = return . Bool $ x `fn` y

binOp :: (LispVal -> ThrowsError a) -> (b -> LispVal) -> (a -> a -> b) -> [LispVal] -> ThrowsError LispVal
binOp unpack pack fn [x, y] = do
    x' <- unpack x
    y' <- unpack y
    return . pack $ x' `fn` y'
binOp _ _ _ args = throwError $ NumArgs 2 args

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "bool" notBool

main :: IO ()
main = do
    args <- getArgs
    evaled <- return . liftM show $ readExpr (args !! 0) >>= eval
    putStrLn . extractValue $ trapError evaled
