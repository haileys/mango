import System.IO
import System.Environment
import Control.Monad
import qualified Mango.Reader as R
import Mango.Eval
import Mango.Value
import Mango.Prelude
import Mango.Exception
import Data.Map (fromList)

protect :: IO a -> IO ()
protect = catchMangoError print . void

replLine :: Scope -> IO ()
replLine scope = do
    putStr ">> "
    hFlush stdout
    code <- getLine
    protect $ do
        val <- evalMany scope $ R.read "(stdin)" code
        putStrLn $ "=> " ++ show val

banner =
    "    _ __ ___   __ _ _ __   __ _  ___    \n" ++
    "   | '_ ` _ \\ / _` | '_ \\ / _` |/ _ \\  \n" ++
    "   | | | | | | (_| | | | | (_| | (_) | \n" ++
    "   |_| |_| |_|\\__,_|_| |_|\\__, |\\___/  \n" ++
    "                           __/ |       \n" ++
    "                          |___/        "

repl :: Scope -> IO ()
repl scope = do
    putStrLn banner
    forever $ replLine scope

run :: Scope -> [String] -> IO ()
run scope (source:argv) = protect $ R.readFile source >>= evalMany scope

createGlobalScope :: IO Scope
createGlobalScope = do
    globalScope <- initPrelude
    exprs <- R.readFile "prelude.mang"
    evalMany globalScope exprs
    return globalScope

main :: IO ()
main = do
    scope <- createGlobalScope
    args <- getArgs
    if null args
        then repl scope
        else run scope args
