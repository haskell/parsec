module Main (main) where
  
import Text.Parsec
import System.Environment (getArgs)
import Control.Monad (replicateM_)

main :: IO ()
main = do
  n <- getArgs >>= \args -> return $ case args of
      arg : _ -> read arg
      _       -> 1000000

  print $ runParser (replicateM_ n $ return ()) () "test" ""
