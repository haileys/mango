import System.IO
import Control.Monad
import qualified Mango.Reader as R
import Mango.Eval
import Mango.Value
import Mango.Prelude
import Mango.Exception
import Data.Map (fromList)

replLine :: Scope -> IO ()
replLine scope = do
    putStr ">> "
    hFlush stdout
    code <- getLine
    catchMangoError print $ do
        val <- evalMany scope $ R.read "(stdin)" code
        putStrLn $ "=> " ++ show val

main :: IO ()
main = do
    globalScope <- initPrelude
    exprs <- R.readFile "prelude.mang"
    evalMany globalScope exprs
    forever $ replLine globalScope