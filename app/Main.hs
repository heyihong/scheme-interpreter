module Main where

import System.Environment
import System.IO
import Control.Monad

import Core
import Parse
import Eval

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt str = flushStr str >> getLine

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map (makeFunc PrimitiveFunc) primitives ++ map (makeFunc IOFunc) ioPrimitives)
	where makeFunc ctr (var, func) = (var, ctr func)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
      else action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne (filename:args) = do
	env <- primitiveBindings >>= flip bindVars [("args", List $ map String args)]
	(runIOThrows $ liftM show $ eval env (List [Atom "load", String filename])) >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

main :: IO ()
main = do
	args <- getArgs
	if null args then runRepl else runOne args

