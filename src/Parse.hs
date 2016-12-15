module Parser
    ( readExpr 
    ) where
import Text.ParserCombinators.Parsec 
import Control.Monad

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces1 :: Parser()
spaces1 = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = do 
				char '"'
				x <- many (noneOf "\"")
				char '"'
				return $ String x

parseAtom :: Parser LispVal
parseAtom = do 
				first <- letter <|> symbol
				rest <- many (letter <|> symbol <|> digit)
				let atom = first:rest
				return $ case atom of
							"#t" -> Bool True
							"#f" -> Bool False
							_ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) (many1 digit)

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

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

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> error $ show err
    Right val -> val

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal