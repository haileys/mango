module Mango.Eval where

import Mango.Value
import Control.Monad
import qualified Data.Map as Map

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
        _                   -> error $ "Attempted to call non-callable: " ++ show callee

eval ctx (MangoNumber   num)    = return $ MangoNumber num
eval ctx (MangoSymbol   sym)    = getVar sym ctx
eval ctx (MangoFunction fn)     = return $ MangoFunction fn
eval ctx (MangoSpecial  spec)   = return $ MangoSpecial spec
eval ctx (MangoQuote    val)    = return val
