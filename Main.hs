import Prelude hiding (showList)
import Numeric
import Control.Monad (liftM)
import System.Environment (getArgs)

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

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lieb" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

eval :: LispVal -> LispVal
eval val@(Char _) = val
eval val@(Bool _) = val
eval val@(String _) = val
eval val@(Float _) = val
eval val@(Number _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom fn : args)) = apply fn $ map eval args

apply :: String -> [LispVal] -> LispVal
apply fn args = maybe (Bool False) ($ args) $ lookup fn primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericFn (+) 0),
    ("-", numericFn' (-) negate),
    ("*", numericFn (*) 1),
    ("/", numericBinOp div),
    ("mod", numericBinOp mod),
    ("quotient", numericBinOp quot),
    ("remainder", numericBinOp rem)]

numericFn :: (Integer -> Integer -> Integer) -> Integer -> [LispVal] -> LispVal
numericFn fn defaultVal [] = Number defaultVal
numericFn fn _ args = numericBinOp fn args

numericFn' :: (Integer -> Integer -> Integer) -> (Integer -> Integer) -> [LispVal] -> LispVal
numericFn' _ oneArgFn [val] = Number . oneArgFn $ unpackNumber val
numericFn' fn _ args = numericBinOp fn args

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinOp fn args = Number $ foldl1 fn $ map unpackNumber args

unpackNumber :: LispVal -> Integer
unpackNumber (Number n) = n
unpackNumber val = error $ "Expected a number, got a " ++ show val

main :: IO ()
main = do
    args <- getArgs
    putStrLn . show $ (readExpr (args !! 0))
