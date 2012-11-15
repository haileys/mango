module Mango.Reader where

import Data.List
import Mango.Value
import Text.ParserCombinators.Parsec

readSymbol :: Parser MangoValue
readSymbol = do
        f <- firstAllowed
        r <- many (firstAllowed <|> digit)
        many space
        return $ MangoSymbol (f:r)
    where firstAllowed = oneOf "+-*/" <|> letter

readNumber :: Parser MangoValue
readNumber = do
    whole <- many1 digit
    many space
    return $ MangoNumber $ (Prelude.read whole :: Double)

readSexp :: Parser MangoValue
readSexp = do
    char '('
    many space
    exprs <- many readExpr
    char ')'
    many space
    return $ MangoList exprs

readExpr :: Parser MangoValue
readExpr = do
    expr <- readSymbol <|> readNumber <|> readSexp
    many space
    return expr

readProgram :: Parser [MangoValue]
readProgram = do
    many space
    exprs <- many readExpr
    eof
    return exprs

read = parse readProgram