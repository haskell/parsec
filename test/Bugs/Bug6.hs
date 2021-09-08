
module Bugs.Bug6
       ( main
       ) where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Parsec
import Text.Parsec.String

import Util

main :: TestTree
main =
  testCase "Look-ahead preserving error location (#6)" $
  parseErrors variable "return" @?= ["'return' is a reserved keyword"]

variable :: Parser String
variable = do
      x <- lookAhead (many1 letter)
      if x == "return"
       then fail "'return' is a reserved keyword"
       else string x
