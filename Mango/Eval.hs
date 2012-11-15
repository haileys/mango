module Mango.Eval where

import Mango.Value
import Mango.Exception
import Control.Monad
import qualified Mango.MutableMap as M

evalMany :: Scope -> [MangoValue] -> IO MangoValue
evalMany _      []  = return $ MangoList []
evalMany scope  xs  = last `liftM` mapM (eval scope) xs

eval :: Scope -> MangoValue -> IO MangoValue

eval ctx (MangoList     [])     = return $ MangoList []

eval ctx (MangoList     (x:xs)) = do
    callee <- eval ctx x
    case callee of
        MangoFunction fn    -> mapM (eval ctx) xs >>= fn
        MangoSpecial spec   -> spec ctx xs
        _                   -> mangoError $ "Attempted to call non-callable: " ++ show callee

eval ctx (MangoNumber   num)    = return $ MangoNumber num
eval ctx (MangoSymbol   sym)    = getVar sym ctx
eval ctx (MangoFunction fn)     = return $ MangoFunction fn
eval ctx (MangoSpecial  spec)   = return $ MangoSpecial spec
eval ctx (MangoQuote    val)    = return val
eval ctx MangoTrue              = return MangoTrue

hasVar :: String -> Scope -> IO Bool
hasVar name RootScope           = return False
hasVar name (Scope parent vars) = do
    exists <- M.member name vars
    case exists of
        True ->     return $ True
        False ->    hasVar name parent

getVar :: String -> Scope -> IO MangoValue
getVar name RootScope           = mangoError $ "Undefined variable '" ++ name ++ "'"
getVar name (Scope parent vars) = do
    val <- M.lookup name vars
    case val of
        Just v ->   return v
        Nothing ->  getVar name parent

setVar :: String -> MangoValue -> Scope -> IO ()
setVar name val RootScope           = error "Panic: Attempted to set a variable on RootScope"
setVar name val (Scope parent vars) = do
    exists <- hasVar name parent
    case exists of
        True ->     setVar name val parent
        False ->    M.insert name val vars
