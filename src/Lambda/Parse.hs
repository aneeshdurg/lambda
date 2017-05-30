{-# LANGUAGE FlexibleContexts #-}
module Lambda.Parse where

import Lambda.Core

import Data.Functor.Identity
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Text.Parsec.Prim hiding (State, try)
import Control.Monad

type Parser = ParsecT String () Identity

--- ### Lexers

parseWith :: Parser a -> String -> Either ParseError a
parseWith p = parse p ""

digitP :: Parser Char
digitP = oneOf ['0'..'9']

digitsP :: Parser String
digitsP = many1 digitP

maybeSpaceP :: Parser String
maybeSpaceP = many $ oneOf " \n\t"

spaceP :: Parser String
spaceP = many1 $ oneOf " \n\t"

idP :: Parser String
idP = liftM2 (:) identFirst (many identRest)
  where identFirst = oneOf $ ['a'..'z'] ++ ['A'..'Z']
        identRest  = identFirst <|> digitP

binOpP :: Parser Val
binOpP = arith <|> isEq <|> logic
  where arith = do c <- oneOf "-*+/><"
                   return $ BinOp c
        isEq = do c <- string "=="
                  return $ BinOp 'e'
        logic = do c <- oneOf "&|"
                   char c
                   case c of
                     '&' -> return $ BinOp 'a'
                     '|' -> return $ BinOp 'o'
--- ### Value parsers

symP :: Parser Val
symP = Symbol <$> idP

-- Parses list and dotted list
lambdaBodyP :: Parser Val
lambdaBodyP = do args <- rawExprP' `sepEndBy` maybeSpaceP
                 char '.' >> maybeSpaceP
                 body <- rawExprP' `sepEndBy` maybeSpaceP
                 return (Lambda args body) 
-- Parses lists
lambdaP :: Parser Val
lambdaP = do char '\\' >> maybeSpaceP
             result <- lambdaBodyP
             return result

parensP :: Parser Val
parensP = do char '(' >> maybeSpaceP
             result <- rawExprP' `sepEndBy` maybeSpaceP
             maybeSpaceP >> char ')'
             return (Parens result)

assignP = try $ do s <- idP 
                   maybeSpaceP >> char '=' >> maybeSpaceP
                   v <- rawExprP' `sepEndBy` maybeSpaceP
                   return $ Assign (Symbol s) v

numP :: Parser Val
numP = Number . read <$> digitsP

boolP :: Parser Val
boolP = char '#' >> Boolean <$> boolLitP
  where boolLitP = const True <$> char 't'
               <|> const False <$> char 'f'
               <?> "a boolean (#f or #t)"

rawExprP :: Parser Val
rawExprP = numP
       <|> assignP
       <|> binOpP 
       <|> symP
       <|> boolP
       <|> parensP
       <|> lambdaP
       <?> "a value"

rawExprP' :: Parser Val
rawExprP' = numP
        <|> binOpP 
        <|> symP
        <|> boolP
        <|> parensP
        <|> lambdaP
        <?> "a value"

transformList :: [Val] -> [Val]
transformList xs = transformH xs []
  where transformH [] res = (reverse . (map transformExpr)) res 
        transformH (x:xs) (BinOp c:v:res) = transformH xs $ (AppBinOp c v x) : res
        transformH ((BinOp c):xs) res = transformH xs ((BinOp c):res)
        transformH (x:xs) ((AppExp f args) : res) = transformH xs ((AppExp f (x:args)):res)
        transformH (x:xs) (f:res) = transformH xs ((AppExp f (x:[])):res)
        transformH (x:xs) [] = transformH xs (x:[])
        
        transformExpr = (flattenLambda . fixApp . fixParens . fixAssign)

        fixApp x = case x of 
                     Lambda args body -> Lambda args $ map fixApp body
                     Parens vs -> Parens $ map fixApp $ transformList vs
                     AppBinOp c v1 v2 -> AppBinOp c (fixApp v1) (fixApp v2)
                     AppExp f args -> AppExp f $ reverse args
                     Assign v1 v2 -> Assign (fixApp v1) (map fixApp v2)
                     _ -> x
       
        fixParens x = case x of 
                     Lambda args body -> Lambda args $ map fixParens body
                     Parens vs -> Parens $ transformList vs
                     AppBinOp c v1 v2 -> AppBinOp c (fixParens v1) (fixParens v2)
                     AppExp f args -> AppExp (fixParens f) $ (map fixParens args)
                     Assign v1 v2 -> Assign (fixParens v1) (map fixParens v2)
                     _ -> x 
 
        fixAssign x = case x of 
                     Assign v1 v2 -> Assign v1 (transformList v2)
                     Lambda args body -> Lambda args (transformList body)
                     _ -> x

        flattenLambda (Lambda args [Lambda args' body]) = flattenLambda $ Lambda (args ++ args') body
        flattenLambda x = x
exprP :: Parser [Val]
exprP = do vals <- between maybeSpaceP maybeSpaceP (rawExprP `sepEndBy` maybeSpaceP) <* eof
           return $ transformList vals
