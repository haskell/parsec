-- this should be fast
module Main (main) where

import Control.DeepSeq (NFData (..))
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import Test.Tasty (defaultMain)
import Test.Tasty.HUnit (testCaseSteps, assertBool)

import Text.Parsec
import Text.Parsec.String (Parser)

main :: IO ()
main = defaultMain $ testCaseSteps "issue-171" $ \info -> do
  time0 <- getCPUTime
  check $ concat $ replicate 100000 "a "
  time1 <- getCPUTime
  let diff = (time1 - time0) `div` 1000000000
  info $ printf "%d milliseconds\n" diff
  assertBool "" (diff < 200)

parser :: Parser [String]
parser = many (char 'a' <|> char 'b') `sepBy` char ' '

check :: String -> IO ()
check s = putStrLn $ either onError (const "") $ parse parser {- important: pass input as SourceName -} s s

onError :: ParseError -> String
onError err = rnf (show err) `seq` "error"
