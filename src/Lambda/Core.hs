{-# LANGUAGE FlexibleContexts #-}

--- Given Code
--- ==========

module Lambda.Core where

import Prelude hiding (lookup)
import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList)
import Data.Typeable
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe

--- ### Environment
type Env = H.HashMap String Val

--- ### Value
data Val = Symbol String
         | Boolean Bool
         | BinOp Char 
         | Number Int
         | Lambda [Val] [Val]
         | Parens [Val]
         | AppBinOp Char Val Val
         | AppExp Val [Val]
         | Assign Val [Val]
         | Void                               -- No value
         deriving (Typeable, Eq)

instance Show Val where
  show (Symbol sym)     = sym
  show (Number i)       = show i
  show (Boolean b)      = if b then "#t" else "#f"
  show (Lambda args bs)  = "λ " ++ (unwords.map show) args ++ " . "++ (unwords.map show) bs
  show (Parens args)  = "(" ++ (unwords.map show) args ++")"
  show (Assign (Symbol s) v) = "[ "++s++" = "++show v++"]"
  show (BinOp op) = op:[]
  show (AppBinOp op v1 v2) = "{" ++ show v1 ++ op:[] ++ show v2 ++ "}"
  show (AppExp f xs) = "|" ++ show f ++ ":" ++ show xs ++ "|"
  show Void             = ""

showArgs :: [Val] -> String
showArgs = unwords . map show

typeName :: Val -> String
typeName Symbol{} = "Symbol"
typeName Number{} = "Number"
typeName Boolean{} = "Boolean"
typeName Lambda{} = "Lambda"
typeName Void = "Void"
typeName (Parens [x]) = typeName x
typeName _ = "MISC"
--- ### Diagnostic

--- In our monadic evaluator, all you need to throw an diagnostic message is to
--- call `throwError`, e.g. `throwError $ NotASymbol v`. Pick the right diagnosic
--- to throw!
data Diagnostic = UnexpectedArgs [Val]
                | TypeError Val
                | NotFuncError Val
                | UndefSymbolError String
                | NotArgumentList Val
                | InvalidSpecialForm String Val
                | CannotApply Val [Val]
                | InvalidExpression Val
                | NotASymbol Val
                | Unimplemented String

instance Show Diagnostic where
  show (UnexpectedArgs actual) =
    "Error: Unexpected arguments or wrong number of arguments (" ++ unwords (map show actual) ++ ")"
  show (TypeError v) =
    "Error: Value " ++ show v ++ " has unexpected type " ++ typeName v
  show (NotFuncError val) =
    "Error: " ++ show val ++ " is not a function"
  show (UndefSymbolError name) =
    "Error: Symbol " ++ name ++ " is undefined"
  show (NotArgumentList val) =
    "Error: Expecting an argument list, but found " ++ show val
  show (InvalidSpecialForm f val) =
    "Error: Invalid pattern in special form `" ++ f ++ "`: " ++ show val
  show (CannotApply val args) =
    "Error: Cannot apply " ++ show val ++ " on argument list (" ++ unwords (map show args) ++ ")"
  show (InvalidExpression val) =
    "Error: Invalid expression: " ++ show val
  show (NotASymbol val) =
    "Error: Not a symbol: " ++ show val
  show (Unimplemented feature) =
    "Error: " ++ feature ++ " is not implemented. You should implement it first!"

-- ### Evaluation monad

-- `StateT` is the monad transformer version of `State`. You do not need to
-- understand monad transformers! Simply read the following declaration as:
-- `EvalState` is a state encapsulating the evaluation result of type `a` and
-- the environment of type `Env`, except when a `Diagnostic` is thown along the
-- evaluation
type EvalState a = StateT Env (Except Diagnostic) a

-- Throw an 'Unimplemented' error with a feature name
unimplemented :: String -> EvalState a
unimplemented = throwError . Unimplemented
