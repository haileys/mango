module Mango.Reader where

import Mango.Value
import Mango.Exception
import Data.List
import Control.Monad
import Text.ParserCombinators.Parsec

comment = do
    char ';'
    manyTill anyChar (void (char '\n') <|> (lookAhead eof))

whitespace = many1 $ oneOf " \t\f\v\n\r"

ignored :: Parser ()
ignored = void $ many $ whitespace <|> comment

readSymbol :: Parser MangoValue
readSymbol = do
        f <- firstAllowed
        r <- many (firstAllowed <|> digit)
        ignored
        return $ MangoSymbol (f:r)
    where firstAllowed = oneOf "+-*/#<>=" <|> letter

readNumber :: Parser MangoValue
readNumber = do
    whole <- many1 digit
    ignored
    return $ MangoNumber $ Prelude.read whole

readSexp :: Parser MangoValue
readSexp = do
    char '('
    ignored
    exprs <- many readExpr
    char ')'
    ignored
    return $ MangoList exprs

readQuote :: Parser MangoValue
readQuote = do
    char '\''
    expr <- readExpr
    return $ MangoQuote expr

escapedStringChar = do
    char '\\'
    c <- anyToken
    return $ case c of
        'b'         -> '\b'
        'n'         -> '\n'
        'f'         -> '\f'
        'v'         -> '\v'
        'r'         -> '\r'
        't'         -> '\t'
        _           -> c

stringChar = do
    escapedStringChar <|> satisfy ('"' /=)

readString :: Parser MangoValue
readString = do
    char '"'
    str <- many stringChar
    char '"'
    ignored
    return $ MangoString str

readExpr :: Parser MangoValue
readExpr = do
    expr <- readSymbol <|> readNumber <|> readSexp <|> readQuote <|> readString
    ignored
    return expr

readProgram :: Parser [MangoValue]
readProgram = do
    ignored
    exprs <- many readExpr
    eof
    return exprs

read file src =
    case parse readProgram file src of
        Left err    -> mangoError $ "Syntax Error in " ++ (show err)
        Right ast   -> ast

readFile file = Mango.Reader.read file `liftM` Prelude.readFile file
