{-# LANGUAGE DeriveDataTypeable #-} -- because haskell is stupid

module Mango.Exception (
    MangoException,
    mangoError,
    catchMangoError,
    backtraceFrame
) where

import Control.Exception
import Data.Typeable
import Prelude hiding (catch)

newtype Backtrace = Backtrace [String]

instance Show Backtrace where
    show (Backtrace xs) = unlines $ map ("  at " ++) xs

data MangoException
    = MangoException String Backtrace
    deriving (Typeable)

instance Exception MangoException

instance Show MangoException where
    show (MangoException msg backtrace) =
        "Exception: " ++ msg ++ "\n" ++ show backtrace

mangoError :: String -> a
mangoError str = throw $ MangoException str (Backtrace [])

catchMangoError :: (MangoException -> IO a) -> IO a -> IO a
catchMangoError = flip catch

backtraceFrame :: String -> IO a -> IO a
backtraceFrame name c = catch c $ \(MangoException msg (Backtrace backtrace)) ->
    throw $ MangoException msg (Backtrace (name:backtrace))