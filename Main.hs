import Numeric
import Control.Monad (liftM)
import System.Environment (getArgs)

import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool
    deriving (Show)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

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

parseNumber :: Parser LispVal
parseNumber = do
    char '#'
    radix <- oneOf "bodx"
    num <- case radix of
        'b' -> many1 (oneOf "01") >>= parseWithRead readBin
        'o' -> many1 (oneOf "01234567") >>= parseWithRead readOct
        'd' -> many1 digit >>= parseWithRead readDec
        'x' -> many1 (digit <|> oneOf "abcdef") >>= parseWithRead readHex
    return $ Number num

parseExpr :: Parser LispVal
parseExpr = parseNumber
    <|> parseAtom
    <|> parseString

readExpr :: String -> String
readExpr input = case parse parseExpr "lieb" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))
