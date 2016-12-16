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

evalAndPrint :: String -> IO ()
evalAndPrint str = putStrLn $ extractValue $ trapError (liftM show $ readExpr str >>= eval)

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
      else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

main :: IO ()
main = do
	args <- getArgs
	case length args of 
		0 -> runRepl
		1 -> evalAndPrint $ args !! 0
		otherwise -> putStrLn "Program takes only 0 or 1 argument"

