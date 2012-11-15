import System.IO
import Control.Monad
import qualified Mango.Reader
import Mango.Eval
import Mango.Value
import Mango.Prelude
import Data.Map (fromList)

replLine :: Scope -> IO ()
replLine scope = do
    putStr ">> "
    hFlush stdout
    code <- getLine
    case Mango.Reader.read "(stdin)" code of
        Left err -> print err
        Right exprs -> do
            val <- evalMany scope exprs
            putStrLn $ "=> " ++ show val

main :: IO ()
main = do
    globalScope <- initPrelude
    forever $ replLine globalScope