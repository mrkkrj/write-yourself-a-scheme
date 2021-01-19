--
-- operator-parser
--
module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad 
import Control.Monad.Error 

main :: IO ()
--main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)
main = do
    args <-getArgs
    -- test:
--    let evaled' =  readExpr  (args !! 0) >>= eval
--    evaled'' <- return $ liftM show $ evaled'
	--
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled

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
         Bool False -> eval alt
         otherwise -> eval conseq

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
               ( "string>=?" , strBoolBinop (>=) ) 
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

isSymbol :: [ LispVal ] -> ThrowsError LispVal
isSymbol ( [Atom _]) = return $ Bool True
isSymbol _ = return $ Bool False

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

instance Show LispError where show = showError

type ThrowsError = Either LispError

trapError action = catchError action ( return . show )

extractValue :: ThrowsError a -> a
extractValue (Right val ) = val

