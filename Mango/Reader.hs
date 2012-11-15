module Mango.Reader where

import Data.List
import Mango.Value
import Control.Monad
import Text.ParserCombinators.Parsec

comment :: Parser ()
comment = do
    char ';'
    manyTill anyChar (void (char '\n') <|> (lookAhead eof))
    return ()

whitespace :: Parser ()
whitespace = void $ many1 space

ignored :: Parser ()
ignored = optional $ whitespace <|> comment

readSymbol :: Parser MangoValue
readSymbol = do
        f <- firstAllowed
        r <- many (firstAllowed <|> digit)
        ignored
        return $ MangoSymbol (f:r)
    where firstAllowed = oneOf "+-*/" <|> letter

readNumber :: Parser MangoValue
readNumber = do
    whole <- many1 digit
    ignored
    return $ MangoNumber $ (Prelude.read whole :: Double)

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

readExpr :: Parser MangoValue
readExpr = do
    expr <- readSymbol <|> readNumber <|> readSexp <|> readQuote
    ignored
    return expr

readProgram :: Parser [MangoValue]
readProgram = do
    ignored
    exprs <- many readExpr
    eof
    return exprs

read = parse readProgram