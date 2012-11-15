module Mango.Value where

import qualified Mango.MutableMap as M

data Scope
    = RootScope
    | Scope { parent :: Scope, vars :: M.MutableMap String MangoValue }

data MangoValue = MangoList     [MangoValue]
                | MangoNumber   Double
                | MangoSymbol   String
                | MangoFunction ([MangoValue] -> IO MangoValue)
                | MangoSpecial  (Scope -> [MangoValue] -> IO MangoValue)
                | MangoQuote    MangoValue

instance Show MangoValue where
    show (MangoList     xs)     = "(" ++ unwords (map show xs) ++ ")"
    show (MangoNumber   num)    = show num
    show (MangoSymbol   sym)    = sym
    show (MangoFunction fun)    = "<function>"
    show (MangoSpecial  spec)   = "<special>"
    show (MangoQuote    val)    = '\'' : show val

hasVar :: String -> Scope -> IO Bool
hasVar name RootScope           = return False
hasVar name (Scope parent vars) = do
    exists <- M.member name vars
    case exists of
        True ->     return $ True
        False ->    hasVar name parent

getVar :: String -> Scope -> IO MangoValue
getVar name RootScope           = error $ "Undefined variable '" ++ name ++ "'"
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