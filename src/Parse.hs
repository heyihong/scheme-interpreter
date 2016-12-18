module Parse
    ( readExpr 
    , readExprList
    ) where

import Core
import Text.ParserCombinators.Parsec 
import Data.Char
import Numeric
import Control.Monad
import Control.Monad.Error

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces1 :: Parser()
spaces1 = skipMany1 space

parseChar :: Parser Char
parseChar = do  
				x <- noneOf "\""
				if x /= '\\' then return x
				else (do 
					y <- anyChar
					case y of 
						'r' -> return '\r'
						'n' -> return '\n'
						't' -> return '\t'
						'\\' -> return '\\'
						'\"' -> return '\"'
						_ -> fail $ "unexpected backslash char \\" ++ [y])


parseString :: Parser LispVal
parseString = do 
				char '"'
				x <- many parseChar 
				char '"'
				return $ String x

parseBool :: Parser LispVal
parseBool = do
				x <- try (sequence [char '#', oneOf "tf"])
				return $ Bool (x == "#t")

parseAtom :: Parser LispVal
parseAtom = do 
				first <- letter <|> symbol
				rest <- many (letter <|> symbol <|> digit)
				let atom = first:rest
				return $ Atom atom

parseNumber :: Parser LispVal
parseNumber = do
				base <- try (sequence [char '#', oneOf "bodxBODX"]) <|> return "#d"
				let di = case (map toLower base) of
							"#b" -> oneOf "01"
							"#o" -> oneOf "01234567"
							"#d" -> digit
							"#x" -> digit <|> (oneOf "abcdefABCDEF")
				spaces
				str <- many1 di
				let num = case (map toLower base) of
							"#b" -> foldr (\x acc -> acc * 2 + if x == '1' then 1 else 0) 0 str
							"#o" -> fst (readOct str !! 0)
							"#d" -> read str
							"#x" -> fst (readHex str !! 0)
				return $ Number num

parseExpr :: Parser LispVal
parseExpr = parseBool
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x
         <|> parseAtom

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces1 

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces1
    tail <- char '.' >> spaces1 >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
     Left err -> throwError $ Parser err
     Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

