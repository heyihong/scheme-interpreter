module Eval  
    ( eval
    , primitives
    , ioPrimitives
    ) where

import Parse
import Core
import Control.Monad.Except
import System.IO

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs varargs = makeFunc (Just (showVal varargs))

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval env (Atom str) = getVar env str
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "set!", Atom var, expr]) = eval env expr >>= setVar env var
eval env (List [Atom "define", Atom var, expr]) = eval env expr >>= defineVar env var
eval env (List ((Atom "define"):List (Atom funcName : params):body)) = makeNormalFunc env params body >>= defineVar env funcName
eval env (List ((Atom "define"):DottedList (Atom funcName : params) varargs:body)) = makeVarArgs varargs env params body >>= defineVar env funcName
eval env (List ((Atom "lambda"):(List params):body)) = makeNormalFunc env params body
eval env (List [Atom "load", String filename]) = load filename >>= liftM last . mapM (eval env)
eval env (List [Atom "if", pred, conseq, alt]) =
	do result <- eval env pred
	   case result of 
	   		Bool True 	-> eval env conseq
	   		Bool False 	-> eval env alt
	   		_ 			-> throwError $ TypeMismatch "boolean" result
eval env (List (Atom funcName : args)) = do
	func <- getVar env funcName
	argVals <- mapM (eval env) args
	apply func argVals

eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
      if length params > length args || length params /= length args && varargs == Nothing
         then throwError $ NumArgs (toInteger $ length params) args
         else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
      where remainingArgs = drop (length params) args
            evalBody env = liftM last $ mapM (eval env) body
            bindVarArgs arg env = case arg of
                Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                Nothing -> return env
apply (IOFunc func) args = func args

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
              ("cons", cons),
              ("eqv?", eqv),
              ("eq?", eqv)]

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
cdr [badArg]				= throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List xs] 				= return $ List (x:xs)
cons [x, DottedList xs xlast]	= return $ DottedList (x:xs) xlast
cons [x1, x2]					= return $ DottedList [x1] x2
cons badArgList              	= throwError $ NumArgs 1 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] 		= return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] 	= return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2]		= return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2]	= return $ Bool $ arg1 == arg2
eqv [List arg1, List arg2]		= return $ Bool $ length arg1 == length arg2 
								&& (all eqvPair $ zip arg1 arg2)
	where eqvPair (x1, x2) 		= case eqv [x1, x2] of
									Left err -> False
									Right (Bool val) -> val
eqv [_, _]						= return $ Bool $ False
eqv badArgList              	= throwError $ NumArgs 1 badArgList

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

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func:args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
