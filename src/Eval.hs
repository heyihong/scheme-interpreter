module Eval  
    ( eval
    ) where

import Core
import Control.Monad.Except

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) =
	do result <- eval pred
	   case result of 
	   		Bool False -> eval alt
	   		otherwise -> eval conseq
eval (List (Atom func : args)) = (mapM eval args) >>= (apply func)
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
						($ args) 
						(lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
			  ("<", numBoolBinop (<)),
			  (">", numBoolBinop (>)),
			  ("/=", numBoolBinop (/=)),
			  (">=", numBoolBinop (>=)),
			  ("<=", numBoolBinop (<=)),
			  ("&&", boolBoolBinop (&&)),
			  ("||", boolBoolBinop (||)),
			  ("string=?", strBoolBinop (==)),
			  ("string<?", strBoolBinop (<)),
			  ("string>?", strBoolBinop (>)),
			  ("string<=?", strBoolBinop (<=)),
			  ("string>=?", strBoolBinop (>=)),
              ("symbol?", isSymbol),
              ("string?", isString),
              ("boolean?", isBoolean),
              ("number?", isNumber),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op [singleValue] = throwError $ NumArgs 2 [singleValue]
numericBinop op params =  mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op params 
    | length params /= 2 = throwError $ NumArgs 2 params
    | otherwise =  mapM unpacker params >>= (\ps -> return $ Bool (op (ps !! 0) (ps !! 1))) 

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool


isBoolean :: [LispVal] -> ThrowsError LispVal
isBoolean [Bool _] = return $ Bool True
isBoolean _ = return $ Bool False

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [Atom _] = return $ Bool True
isSymbol _ = return $ Bool False

isString :: [LispVal] -> ThrowsError LispVal
isString [String _] = return $ Bool True
isString _ = return $ Bool False

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [Number _] = return $ Bool True
isNumber _ = return $ Bool False

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] 		= return $ List xs
cdr [DottedList [_] x] 		= return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArgs]				= throwError $ TypeMismatch "pair" badArgs
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List xs] 				= return $ List (x:xs)
cons [x, DottedList xs xlast]	= return $ DottedList (x:xs) xlast
cons [x1, x2]					= return $ DottedList [x1] x2
cons badArgList              	= throwError $ NumArgs 1 badArgList

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                           if null parsed 
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool