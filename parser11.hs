﻿--
-- repl-parser
--
{-# LANGUAGE ExistentialQuantification #-} 
module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad 
import Control.Monad.Error 
import System.IO hiding (try)

main :: IO ()
main = do
    args <-getArgs
    case length args of
      0 -> runRepl
      1 -> evalAndPrint $ args !! 0
      otherwise -> putStrLn "Program takes onla 1ß or 1 args!"

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse (spaces >> parseExpr) " lisp " input of
	Left err -> throwError $ Parser err
	Right val -> return val

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
-- OLD:: parseNumber = liftM (Number . read) $ many1 digit
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
eval :: LispVal -> ThrowsError LispVal
eval val@( String _ ) = return val
eval val@( Number _ ) = return val
eval val@( Bool _ ) = return val
eval ( List [Atom "quote" , val ] ) = return val
eval ( List [Atom "if" , pred , conseq , alt ] ) = 
    do result <- eval pred
       case result of
         Bool True -> eval conseq 
         Bool False -> eval alt
         notBool -> throwError $ TypeMismatch "bolean" notBool

-- excercise: cond
eval ( List (Atom "cond" : clauses) ) =
   let evalClauses clauses = 
         case clauses of 
           x:[] -> case x of 
               List[ Atom "else", val ] -> eval val
               List[ p, v ] -> 
                 do 
                   pred <- eval p
                   case pred of 
                     Bool True -> eval v
                     Bool False -> return $ Bool False -- undefined, might be anything!
                     notBool -> throwError $ TypeMismatch "bolean" notBool 
               badClause -> throwError $ TypeMismatch "list" badClause 
           x:xs -> case x of 
               List[ p, v ] -> 
                 do 
                   pred <- eval p
                   case pred of 
                      Bool True -> eval v
                      Bool False -> evalClauses xs
                      notBool -> throwError $ TypeMismatch "bolean" notBool 
               badClause -> throwError $ TypeMismatch "list" badClause 
           _ -> throwError $ BadSpecialForm "no condition clauses" $ Atom "cond"  
   in 
     evalClauses clauses  						      

-- OLD:: eval ( List (Atom func : args ) ) = apply func $ map eval args
eval ( List (Atom func : args ) ) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [ LispVal ] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) 
                        ($ args) $ lookup func primitives

primitives :: [ ( String , [ LispVal ] -> ThrowsError LispVal ) ]
primitives = [ ( "+" , numericBinop (+) ) ,
               ( "-" , numericBinop (-) ) ,
			   ( "*" , numericBinop (*) ) ,
               ( "/" , numericBinop div ) ,
               ( "mod " , numericBinop mod ) ,
               ( "quotient" , numericBinop quot ) ,
               ( "remainder" , numericBinop rem ) ,
			   ( "symbol?" , isSymbol ), 
			   --( "string?" , isString ) ,
			   --( "number?" , isNum ) ,
               ( "=" , numBoolBinop (==) ) ,
               ( "<" , numBoolBinop (<) ) ,
               ( ">" , numBoolBinop (>) ) ,
               ( "/=" , numBoolBinop (/=) ) ,
               ( ">=" , numBoolBinop (>=) ) ,
               ( "<=" , numBoolBinop (<=) ) ,
               ( "&&" , boolBoolBinop (&&) ) ,
               ( "||" , boolBoolBinop (||) ) ,
               ( "string=?" , strBoolBinop (==) ) ,
               ( "string>?" , strBoolBinop (>) ) ,
               ( "string<=?" , strBoolBinop (<=) ) ,
               ( "string>=?" , strBoolBinop (>=) ) ,
               ( "car" , car ) ,
               ( "cdr" , cdr ) ,
               ( "cons" , cons ) ,
               ( "eq?" , eqv ) ,
               ( "eqv?" , eqv ) ,
               ( "equal?" , equal ) 
			 ]

numericBinop :: ( Integer -> Integer -> Integer ) -> [ LispVal ] -> ThrowsError LispVal
-- OLD:: numericBinop op params = Number $ foldl1 op $ map unpackNum params
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: ( LispVal -> ThrowsError a ) -> ( a -> a -> Bool ) -> [ LispVal ] -> ThrowsError LispVal
boolBinop unpacker op args = 
    if length args /= 2
      then throwError $ NumArgs 2 args
      else do left <- unpacker $ args !! 0 -- in Error monad
              right <- unpacker $ args !! 1
              return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum ( String n) = let parsed = reads n in
                           if null parsed
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum ( List [ n ] ) = unpackNum n
unpackNum notNumbr = throwError $ TypeMismatch "number" notNumbr

unpackStr :: LispVal -> ThrowsError String
unpackStr ( String s ) = return s
unpackStr ( Number s ) = return $ show s
unpackStr ( Bool s ) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString 

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

-- eval : symbol ops
isSymbol :: [ LispVal ] -> ThrowsError LispVal
isSymbol ( [Atom _]) = return $ Bool True
isSymbol _ = return $ Bool False

-- eval : lists
car :: [ LispVal ] -> ThrowsError LispVal
car [ List ( x : xs ) ] = return x
car [ DottedList ( x : xs ) _ ] = return x
car [ badArg ] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [ LispVal ] -> ThrowsError LispVal
cdr [ List ( x : xs ) ] = return $ List xs
cdr [ DottedList ( _: xs ) x ] = return $ DottedList xs x
cdr [ DottedList [ xs ] x ] = return x
cdr [ badArg ] = throwError $ TypeMismatch " pair " badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [ LispVal ] -> ThrowsError LispVal
cons [ x1 , List [ ] ] = return $ List [ x1 ]
cons [ x , List xs ] = return $ List $ [ x ] ++ xs
cons [ x , DottedList xs xlast ] = return $ DottedList ( [ x ] ++ xs ) xlast
cons [ x1 , x2 ] = return $ DottedList [ x1 ] x2
cons badArgList = throwError $ NumArgs 2 badArgList

-- eval : equality
eqv :: [ LispVal ] -> ThrowsError LispVal
eqv [ ( Bool arg1 ) , (Bool arg2 ) ] = return $ Bool $ arg1 == arg2
eqv [ ( Number arg1 ) , (Number arg2 ) ] = return $ Bool $ arg1 == arg2
eqv [ ( String arg1 ) , ( String arg2 ) ] = return $ Bool $ arg1 == arg2
eqv [ (Atom arg1 ) , (Atom arg2 ) ] = return $ Bool $ arg1 == arg2
eqv [ ( DottedList xs x ) , ( DottedList ys y ) ] = eqv [ List $ xs ++ [ x ] , List $ ys ++ [ y ] ]
eqv [ ( List arg1 ) , ( List arg2 ) ] = return $ Bool $ ( length arg1 == length arg2 ) &&
                                                        ( and $ map eqvPair $ zip arg1 arg2 )
                                                 where eqvPair ( x1 , x2 ) = case eqv [ x1 , x2 ] of
                                                                               Left err -> False
                                                                               Right (Bool val ) -> val
eqv [ _,_ ] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

-- eval : week typed equality
data Unpacker = forall a . Eq a => AnyUnpacker ( LispVal -> ThrowsError a ) -- exist. quantification, defines an interface...

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 ( AnyUnpacker unpacker ) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
    `catchError` ( const $ return False )

equal :: [ LispVal ] -> ThrowsError LispVal
-- excercise: equal? bug,  TODO:: factor the list caluse into a separate helper function 
--                                that is parameterized by the equality testing function....
equal [ ( List arg1 ) , ( List arg2 ) ] = return $ Bool $ ( length arg1 == length arg2 ) &&
                                                        ( and $ map eqvPair $ zip arg1 arg2 )
                                                 where eqvPair ( x1 , x2 ) = case equal [ x1 , x2 ] of
                                                                               Left err -> False
                                                                               Right (Bool val ) -> val
equal [ arg1 , arg2 ] = do
    primitiveEquals <- liftM or $ mapM ( unpackEquals arg1 arg2 ) -- try all unpcakers for week typing!
                       [ AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool ]
    eqvEquals <- eqv [ arg1 , arg2 ]
    return $ Bool $ ( primitiveEquals || let ( Bool x ) = eqvEquals in x ) -- let... : extracts x via pattern matching!!!
equal badArgList = throwError $ NumArgs 2 badArgList


-- error handling
data LispError = NumArgs Integer [ LispVal ]
	| TypeMismatch String LispVal
	| Parser ParseError
	| BadSpecialForm String LispVal
	| NotFunction String String
	| UnboundVar String String
	| Default String

instance Error LispError where
	noMsg = Default "An error has occurred"
	strMsg = Default

showError :: LispError -> String
showError ( UnboundVar message varname ) = message ++ ": " ++ varname
showError ( BadSpecialForm message form) = message ++ ": " ++ show form
showError ( NotFunction message func ) = message ++ ": " ++ show func
showError ( NumArgs expected found ) = "Expected" ++ show expected
							++ " args : found values " ++ unwordsList found
showError ( TypeMismatch expected found ) = " Invalid type : expected " ++ expected
							++ " , found " ++ show found
showError ( Default strg) = " ERROR " ++ strg
showError ( Parser parseErr) = show parseErr


instance Show LispError where show = showError

type ThrowsError = Either LispError

trapError action = catchError action ( return . show )

extractValue :: ThrowsError a -> a
extractValue (Right val ) = val


-- REPL
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = 
  return
    $ extractValue 
    $ trapError ( liftM show $ readExpr expr >>= eval )

evalAndPrint :: String -> IO ( )
evalAndPrint expr = evalString expr >>= putStrLn

-- OLD:
--evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
--putStrLn $ extractValue $ trapError evaled

until_ :: Monad m => ( a -> Bool ) -> m a -> ( a -> m ( ) ) -> m ( )
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint