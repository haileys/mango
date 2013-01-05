module Mango.Eval where

import Mango.Value
import Mango.Exception
import Control.Monad
import qualified Mango.MutableMap as M

evalMany :: Scope -> [MangoValue] -> IO MangoValue
evalMany _      []  = return $ MangoList []
evalMany scope  xs  = last `liftM` mapM (eval scope) xs

apply :: Scope -> [MangoValue] -> MangoValue -> IO MangoValue
apply ctx args (MangoFunction    fn)    = mapM (eval ctx) args >>= fn
apply ctx args (MangoSpecial     spec)  = spec ctx args
apply ctx args x                        = mangoError $ "Can't apply " ++ show x

eval :: Scope -> MangoValue -> IO MangoValue
eval ctx (MangoList     [])     = return $ MangoList []
eval ctx (MangoList     (x:xs)) = backtraceFrame ("application of " ++ show x) $ eval ctx x >>= apply ctx xs
eval ctx (MangoNumber   num)    = return $ MangoNumber num
eval ctx (MangoSymbol   sym)    = getVar sym ctx
eval ctx (MangoString   str)    = return $ MangoString str
eval ctx (MangoFunction fn)     = return $ MangoFunction fn
eval ctx (MangoSpecial  spec)   = return $ MangoSpecial spec
eval ctx (MangoQuote    val)    = return val
eval ctx (MangoQuasi    val)    = quasi ctx val
eval ctx (MangoUnquote  val)    = eval ctx val
eval ctx MangoTrue              = return MangoTrue

quasi :: Scope -> MangoValue -> IO MangoValue
quasi ctx (MangoList    xs)     = MangoList `liftM` mapM (quasi ctx) xs
quasi ctx (MangoQuote   val)    = return val
quasi ctx (MangoQuasi   val)    = quasi ctx val
quasi ctx (MangoUnquote val)    = eval ctx val
quasi ctx x                     = return x

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
