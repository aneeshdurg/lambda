module Main where
import Lambda.Core
import Lambda.Parse
import Lambda.Eval

import Prelude hiding (lookup)
import System.IO (hFlush, hPutStr, hPutStrLn, hGetLine, stdin, stdout)
import System.Console.Readline
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import qualified Data.HashMap.Strict as H (empty)

--- ### REPL

printLn :: String -> IO ()
printLn str = hPutStrLn stdout str >> hFlush stdout

repl :: Env -> IO ()
repl env = do
  r <- readline "Lambda> "                            -- Read
  let l = case r of
            Just s -> s
            Nothing -> ""
  case parse exprP "Expression" l of                  -- Parse
    Left err -> print err                             -- Diagnostics
    Right [expr] -> 
      case runExcept $ runStateT (eval expr) env of   -- Eval
        Left err -> print err
        Right (Void, env') -> repl env'
        Right (x, env') -> do print x
                              repl env'
    _ -> print Void
  repl env                                            -- Loop with old env

main :: IO ()
main = repl H.empty


