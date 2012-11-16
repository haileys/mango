module Mango.Prelude where

import Control.Monad
import Mango.Helpers
import Mango.Value
import Mango.Eval
import Mango.Exception
import Mango.MutableMap (insert, fromList)

car :: [MangoValue] -> IO MangoValue
car argv = do
    (list)  <- expectArgs1  argv
    (x:_)   <- expectList   list
    return x

cdr :: [MangoValue] -> IO MangoValue
cdr argv = do
    (list)  <- expectArgs1  argv
    (_:xs)  <- expectList   list
    return $ MangoList xs

_null :: [MangoValue] -> IO MangoValue
_null argv = do
    (list)  <- expectArgs1  argv
    xs      <- expectList   list
    return $ fromBool $ null xs

cons :: [MangoValue] -> IO MangoValue
cons argv = do
    (car,cdr)   <- expectArgs2  argv
    xs          <- expectList   cdr
    return $ MangoList $ car:xs

quote :: Scope -> [MangoValue] -> IO MangoValue
quote _ argv = return $ MangoList argv

list :: [MangoValue] -> IO MangoValue
list = quote RootScope

lambda :: Scope -> [MangoValue] -> IO MangoValue
lambda ctx argv = do
    (args,exprs)    <- expectVArgs1 argv
    argSymbols      <- expectList   args
    argStrings      <- mapM expectSymbol argSymbols
    return $ MangoFunction $ \args -> do
        if length args /= length argStrings
            then mangoError $ "Expected " ++ show (length argStrings) ++ " arguments, received " ++ show (length args)
            else do
                vars <- fromList (zip argStrings args)
                evalMany (Scope ctx vars) exprs

macro :: Scope -> [MangoValue] -> IO MangoValue
macro ctx argv = do
    (args,exprs)    <- expectVArgs1 argv
    argSymbols      <- expectList   args
    argStrings      <- mapM expectSymbol argSymbols
    return $ MangoSpecial $ \callerCtx -> \args -> do
        if length args /= length argStrings
            then mangoError $ "Expected " ++ show (length argStrings) ++ " arguments, received " ++ show (length args)
            else do
                vars <- fromList (zip argStrings args)
                evalMany (Scope ctx vars) exprs

set :: Scope -> [MangoValue] -> IO MangoValue
set ctx argv = do
    (sym,expr)  <- expectArgs2 argv
    var         <- expectSymbol sym
    val         <- eval ctx expr
    setVar var val ctx
    return val

_eval :: Scope -> [MangoValue] -> IO MangoValue
_eval ctx argv = mapM (eval ctx) argv >>= evalMany ctx

_if :: Scope -> [MangoValue] -> IO MangoValue
_if ctx argv = do
    (cond,t,f)      <- expectArgs3 argv
    evaluatedCond   <- eval ctx cond
    eval ctx $ if isTruthy evaluatedCond then t else f

_print :: [MangoValue] -> IO MangoValue
_print argv = do
    (flip mapM) argv $ \val ->
        case val of
            MangoString str -> putStrLn str
            _               -> print val
    return $ MangoTrue

gets :: [MangoValue] -> IO MangoValue
gets argv = do
    expectArgs0 argv
    MangoString `liftM` getLine

eq :: [MangoValue] -> IO MangoValue
eq argv = do
    (a,b)   <- expectArgs2 argv
    return $ fromBool $ a == b

mkMathFunction :: ToMango a => (Double -> Double -> a) -> MangoValue
mkMathFunction op = MangoFunction $ \argv -> do
    (av, bv)    <- expectArgs2 argv
    a           <- expectNumber av
    b           <- expectNumber bv
    return $ toMango $ a `op` b

initPrelude :: IO Scope
initPrelude = do
    globals <- fromList []
    
    insert "car"    (MangoFunction  car)        globals
    insert "cdr"    (MangoFunction  cdr)        globals
    insert "null"   (MangoFunction  _null)      globals
    insert "cons"   (MangoFunction  cons)       globals
    insert "list"   (MangoFunction  list)       globals
    insert "quote"  (MangoSpecial   quote)      globals
    insert "lambda" (MangoSpecial   lambda)     globals
    insert "macro"  (MangoSpecial   macro)      globals
    insert "set"    (MangoSpecial   set)        globals
    insert "eval"   (MangoSpecial   _eval)      globals
    insert "if"     (MangoSpecial   _if)        globals
    insert "#t"     MangoTrue                   globals
    insert "print"  (MangoFunction  _print)     globals
    insert "gets"   (MangoFunction  gets)       globals
    insert "="      (MangoFunction  eq)         globals
    
    insert "+"      (mkMathFunction (+))        globals
    insert "-"      (mkMathFunction (-))        globals
    insert "*"      (mkMathFunction (*))        globals
    insert "/"      (mkMathFunction (/))        globals
    insert "**"     (mkMathFunction (**))       globals
    insert "<"      (mkMathFunction (<))        globals
    insert ">"      (mkMathFunction (>))        globals
    
    return $ Scope RootScope globals