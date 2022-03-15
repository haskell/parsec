module Bugs.Bug9 ( main ) where

import           Control.Applicative            ((<$), (<$>), (<*))
import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language           (haskellStyle)
import           Text.Parsec.String             (Parser)
import qualified Text.Parsec.Token              as P

import           Test.Tasty
import           Test.Tasty.HUnit

import           Util

data Expr = Const Integer | Op Expr Expr
  deriving Show

main :: TestTree
main =
  testCase "Tracing of current position in error message (#9)"
  $ result @?= ["unexpected '>'","expecting operator or end of input"]

  where
    result :: [String]
    result = parseErrors parseTopLevel "4 >> 5"

-- Syntax analysis

parseTopLevel :: Parser Expr
parseTopLevel = parseExpr <* eof

parseExpr :: Parser Expr
parseExpr = buildExpressionParser table (Const <$> integer)
  where
        table = [[ Infix (Op <$ reserved ">>>") AssocLeft ]]

        -- Lexical analysis

        lexer = P.makeTokenParser haskellStyle { P.reservedOpNames = [">>>"] }

        integer    = P.integer    lexer
        reserved   = P.reserved   lexer
        _reservedOp = P.reservedOp lexer

