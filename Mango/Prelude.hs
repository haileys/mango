module Mango.Prelude where

import Mango.Helpers
import Mango.Value
import Mango.Eval
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

quote :: Scope -> [MangoValue] -> IO MangoValue
quote _ argv = return $ MangoList argv

lambda :: Scope -> [MangoValue] -> IO MangoValue
lambda ctx argv = do
    (args,exprs)    <- expectVArgs1 argv
    argSymbols      <- expectList   args
    argStrings      <- mapM expectSymbol argSymbols
    return $ MangoFunction $ \args -> do
        if length args /= length argStrings
            then error $ "Expected " ++ show (length argStrings) ++ " arguments, received " ++ show (length args)
            else do
                vars <- fromList (zip argStrings args)
                evalMany (Scope ctx vars) exprs

mkMathFunction :: (Double -> Double -> Double) -> MangoValue
mkMathFunction op = MangoFunction $ \argv -> do
    (av, bv)    <- expectArgs2 argv
    a           <- expectNumber av
    b           <- expectNumber bv
    return $ MangoNumber $ a `op` b

initPrelude :: IO Scope
initPrelude = do
    globals <- fromList []
    
    insert "car"    (MangoFunction car)     globals
    insert "cdr"    (MangoFunction cdr)     globals
    insert "quote"  (MangoSpecial quote)    globals
    insert "lambda" (MangoSpecial lambda)   globals
    
    insert "+"      (mkMathFunction (+))    globals
    insert "-"      (mkMathFunction (-))    globals
    insert "*"      (mkMathFunction (*))    globals
    insert "/"      (mkMathFunction (/))    globals
    insert "**"     (mkMathFunction (**))   globals
    
    return $ Scope RootScope globals