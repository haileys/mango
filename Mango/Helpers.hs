module Mango.Helpers where
    
import Mango.Value

expectArgs0     :: [MangoValue] -> IO ()
expectArgs0     []              = return ()
expectArgs0     xs              = error $ "Expected no arguments, received " ++ show (length xs)

expectArgs1     :: [MangoValue] -> IO (MangoValue)
expectArgs1     [a]             = return (a)
expectArgs1     xs              = error $ "Expected 1 argument, received " ++ show (length xs)

expectArgs2     :: [MangoValue] -> IO (MangoValue, MangoValue)
expectArgs2     [a,b]           = return (a,b)
expectArgs2     xs              = error $ "Expected 2 arguments, received " ++ show (length xs)

expectVArgs1    :: [MangoValue] -> IO (MangoValue, [MangoValue])
expectVArgs1    (a:rest)        = return (a,rest)
expectVArgs1    xs              = error $ "Expected >= 1 arguments, received " ++ show (length xs)

expectVArgs2    :: [MangoValue] -> IO (MangoValue, MangoValue, [MangoValue])
expectVArgs2    (a:b:rest)      = return (a,b,rest)
expectVArgs2    xs              = error $ "Expected >= 2 arguments, received " ++ show (length xs)

expectList      :: MangoValue -> IO [MangoValue]
expectList      (MangoList x)   = return x
expectList      x               = error $ "Expected list, received " ++ show x

expectNumber    :: MangoValue -> IO Double
expectNumber    (MangoNumber x) = return x
expectNumber    x               = error $ "Expected number, received " ++ show x

expectSymbol    :: MangoValue -> IO String
expectSymbol    (MangoSymbol x) = return x
expectSymbol    x               = error $ "Expected symbol, received " ++ show x