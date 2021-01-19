﻿--
-- evaluator contd. -  Additional Primitives: Partial Application
--
module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad 

main :: IO ()
--main = do args <- getArgs
--          putStrLn (readExpr ( args !! 0 ))
main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

readExpr :: String -> LispVal
readExpr input = case parse (spaces >> parseExpr) " lisp " input of
	Left err -> String $ " No match : " ++ show err
	Right val -> val

spaces :: Parser ()
spaces = skipMany space

data LispVal = Atom String
			 | List [ LispVal ]
			 | DottedList [ LispVal ] LispVal
			 | Number Integer
			 | String String
			 | Bool Bool

-- string
parseString :: Parser LispVal
parseString  = do char '"'
                  x <- many ( noneOf "\"" )
                  char '"'
                  return $ String x

-- atom
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many ( letter <|> digit <|> symbol )
               let atom = [ first ] ++ rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          otherwise -> Atom atom

-- number
parseNumber :: Parser LispVal
--parseNumber = liftM (Number . read) $ many1 digit
parseNumber = do num <- many1 digit
                 return $ (Number . read) num

-- list
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- dotetd list
parseDottedList :: Parser LispVal
parseDottedList = do
	head <- endBy parseExpr spaces
	tail <- char '.' >> spaces >> parseExpr
	return $ DottedList head tail

-- quote
parseQuoted :: Parser LispVal
parseQuoted = do
	char '\''
	x <- parseExpr
	return $ List [Atom "quote" , x ]

-- expr
parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do char '('
           x <- ( try parseList ) <|> parseDottedList
           char ')'
           return x

-- showVal
showVal :: LispVal -> String
showVal ( String contents  ) = "\"" ++ contents ++ "\""
showVal (Atom name ) = name
showVal (Number contents ) = show contents
showVal (Bool True) = "#t "
showVal (Bool False ) = "#f "
showVal ( List contents ) = " ( " ++ unwordsList contents ++ " ) "
showVal ( DottedList head tail ) = " ( " ++ unwordsList head ++ " . " ++ showVal tail ++ " ) "

unwordsList :: [ LispVal ] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

-- eval
eval :: LispVal -> LispVal
eval val@( String _ ) = val
eval val@( Number _ ) = val
eval val@( Bool _ ) = val
eval ( List [Atom "quote" , val ] ) = val
eval ( List (Atom func : args ) ) = apply func $ map eval args

apply :: String -> [ LispVal ] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [ ( String , [ LispVal ] -> LispVal ) ]
primitives = [ ( "+" , numericBinop (+) ) ,
               ( "-" , numericBinop (-) ) ,
			   ( "*" , numericBinop (*) ) ,
               ( "/" , numericBinop div ) ,
               ( "mod " , numericBinop mod ) ,
               ( "quotient" , numericBinop quot ) ,
               ( "remainder" , numericBinop rem ) ,
			   ( "symbol?" , isSymbol ) ,
			   --( "string?" , isString ) ,
			   --( "number?" , isNum ) ,
               ( "= " , numBoolBinop (==) ) ,
               ( "<" , numBoolBinop (<) ) ,
               ( ">" , numBoolBinop (>) ) ,
               ( "/=" , numBoolBinop (/=) ) ,
               ( ">=" , numBoolBinop (>=) ) ,
               ( "<=" , numBoolBinop (<=) ) ,
               ( "&&" , boolBoolBinop (&&) ) ,
               ( "||" , boolBoolBinop (||) ) ,
               ( "string=?" , strBoolBinop (==) ) ,
               ( "string?" , strBoolBinop (>) ) ,
               ( "string<=?" , strBoolBinop (<=) ) ,
               ( "string>=?" , strBoolBinop (>=) ) 
			 ]

numericBinop :: ( Integer -> Integer -> Integer ) -> [ LispVal ] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params


boolBinop :: ( LispVal -> ThrowsError a ) -> ( a −> a −> Bool ) −> [ LispVal ]−> ThrowsError LispVal
boolBinop unpacker op args = 
    if length args /= 2
      then throwError $ NumArgs 2 args
      else do left <− unpacker $ args !! 0 -- in Error monad
              right <- unpacker $ args !! 1
              return $ Bool $ left ´op´ right

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum ( String n) = let parsed = reads n in
                           if null parsed
                             then 0
                             else fst $ parsed !! 0
unpackNum ( List [ n ] ) = unpackNum n
unpackNum _ = 0

isSymbol :: [ LispVal ] -> LispVal
isSymbol ( [Atom _]) = Bool True
isSymbol _ = Bool False