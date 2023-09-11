module Main (main) where

import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.String (Parser)
import Text.Parsec.Pos (newPos)

import Test.Tasty (defaultMain)
import Test.Tasty.HUnit (assertFailure, testCaseSteps, (@?=))

main :: IO ()
main = defaultMain $ testCaseSteps "issue175" $ \info -> do
    case parse p "" "x" of
        Right _ -> assertFailure "Unexpected success"
        -- with setPosition the "longest match" is arbitrary
        -- megaparsec tracks consumed tokens separately, but we don't.
        -- so our position is arbitrary.
        Left err -> do
            info $ show err
            errorPos err @?= newPos "aaa" 9 1  -- can be arbitrary
            length (errorMessages err) @?= 2

p :: Parser Char
p = p1 <|> p2 where
    p1 = setPosition (newPos "aaa" 9 1) >> char 'a'
    p2 = setPosition (newPos "zzz" 1 1) >> char 'b'
