{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Lambda.Eval where

import Lambda.Core

import Prelude hiding (lookup)
import qualified Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList, union)
import Control.Monad.State
import Control.Monad.Except

fixM :: (Val -> EvalState Val) -> Val -> EvalState Val
fixM f x = do x' <- f x
              if x'==x
                then 
                  return x'
                else
                  fixM f x'

resolveSyms :: Val -> EvalState Val
resolveSyms v@(Symbol _) = eval v
resolveSyms v@(Number _) = return v
resolveSyms v@(Boolean _) = return v
resolveSyms (Parens [x]) = do x' <- resolveSyms x
                              return $ Parens [x']
resolveSyms (AppBinOp c v1 v2) = do v1' <- resolveSyms v1
                                    v2' <- resolveSyms v2
                                    return $ AppBinOp c v1' v2'
resolveSyms (AppExp v args) = do v' <- resolveSyms v
                                 args' <- mapM resolveSyms args
                                 return $ AppExp v' args'

resolveSyms (Lambda args [body]) = let env' = H.fromList $ zip (map (\(Symbol s) -> s) args) args
                                   in do env <- get
                                         modify $ H.union env'
                                         body' <- resolveSyms body
                                         put env
                                         return $ Lambda args [body']


eval :: Val -> EvalState Val
eval (Symbol s) = do e <- get
                     case H.lookup s e of
                       Just v -> return v
                       _      -> return (Symbol s)
eval (Assign (Symbol s) [v]) = do v' <- eval v
                                  modify $ H.insert s v'
                                  return Void
eval v@(Boolean _) = return v
eval v@(Number _) = return v
eval v@(Lambda args [body]) =
  let env' = H.fromList $ zip (map (\(Symbol s) -> s) args) args
  in do env <- get                 
        modify $ H.union env' 
        b <- resolveSyms body
        put env
        return $ Lambda args [b]

eval (Parens [x]) = eval x
eval (AppBinOp op v1 v2) =
  do v1' <- eval v1
     case v1' of 
       Number n -> case getArith op of
                     Just f  -> do v2' <- eval v2
                                   case v2' of
                                     Number n' -> return $ f n n'
                                     Boolean _ -> throwError $ TypeError v2
                                     _         -> return $ AppBinOp op v1' v2'
                     Nothing -> do throwError $ TypeError v1

       Boolean b -> case getLogic op of
                      Just f  -> do v2' <- eval v2
                                    case v2' of 
                                      Boolean b' -> return $ f b b'
                                      Number _   -> throwError $ TypeError v2
                                      _          -> return $ AppBinOp op v1' v2'
                      Nothing -> do throwError $ TypeError v1
       l1@(Lambda args [body]) -> case op of 
                                    'e' -> do v2' <- eval v2
                                              case v2' of 
                                                l2@(Lambda args' [body']) -> compareLambda l1 l2
                                                _ -> throwError $ TypeError v2
                                    _   -> do throwError $ TypeError v1
       _         -> do v2' <- eval v2
                       return $ AppBinOp op v1' v2'
  where getArith op = case op of
                         '+' -> Just $ (\n n' -> Number (n+n'))
                         '-' -> Just $ (\n n' -> Number (n-n'))
                         '/' -> Just $ (\n n' -> Number (n`div`n'))
                         '*' -> Just $ (\n n' -> Number (n*n'))
                         '>' -> Just $ (\n n' -> Boolean (n>n'))
                         '<' -> Just $ (\n n' -> Boolean (n<n'))
                         'e' -> Just $ (\n n' -> Boolean (n==n'))
                         _   -> Nothing 
        getLogic op = case op of
                         'a' -> Just $ (\n n' -> Boolean (n&&n'))
                         'o' -> Just $ (\n n' -> Boolean (n||n'))
                         'e' -> Just $ (\n n' -> Boolean (n==n'))
                         _   -> Nothing 
        compareLambda (Lambda a1 [b1]) (Lambda a2 [b2]) = 
          if (length a1 == length a2)
            then
              let env' = H.fromList $ zip (map (\(Symbol s) -> s) a2) a1
              in  do env <- get
                     modify $ H.union env'
                     b2' <- resolveSyms b2
                     put env
                     return $ Boolean (b1==b2')
            else
              return $ Boolean False
eval (AppExp (Symbol s) args) = do e <- get
                                   case H.lookup s e of
                                     Just x -> eval (AppExp x args) 
                                     _      -> return $ AppExp (Symbol s) args 
eval (AppExp (Parens [x]) args) = eval (AppExp x args) 
eval (AppExp (Lambda ((Symbol a):args) [body]) (a':argvals)) = 
  let env' = H.fromList $ zip (map (\(Symbol s) -> s) args) args
  in do env <- get
        modify $ H.union env'
        modify $ H.insert a a'
        b' <- resolveSyms body
        put env
        eval (AppExp (Lambda args [b']) argvals)


eval (AppExp (Lambda (a:args) [body]) []) = eval $ Lambda (a:args) [body]
eval v@(AppExp (Lambda [] [body]) (a:argvals)) = eval (AppExp body (a:argvals)) 
eval (AppExp (Lambda [] [body]) []) = fixM eval body

eval x = throwError $ InvalidExpression x
