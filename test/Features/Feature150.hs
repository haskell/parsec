module Features.Feature150 ( main ) where

import           Control.Applicative ((*>))
import           Control.Monad.Identity
import           Test.Tasty
import           Test.Tasty.HUnit

import           Text.Parsec

main :: TestTree
main =
  testCase "string' (#150)" $ do
    parseString (boot <|> bool) "boot" @?= "boot"
    parseFail   (boot <|> bool) "bool" @?= "no parse"
    parseFail   (boot <|> bool) "booz" @?= "no parse"

    parseString (boot' <|> bool') "boot" @?= "boot"
    parseString (boot' <|> bool') "bool" @?= "bool"
    parseFail   (boot' <|> bool') "booz" @?= "no parse"

    parseString (boot' <|> bool' <|> char 'b' *> many anyChar) "boomerang" @?= "oomerang"

 where
    boot :: ParsecT String () Identity String
    boot = string "boot"

    bool :: ParsecT String () Identity String
    bool = string "bool"

    boot' :: ParsecT String () Identity String
    boot' = string' "boot"

    bool' :: ParsecT String () Identity String
    bool' = string' "bool"

    parseString :: ParsecT String () Identity String -> String -> String
    parseString p input =
      case parse p "Example" input of
        Left{}    -> error "Parse failure"
        Right str -> str

    parseFail :: ParsecT String () Identity String -> String -> String
    parseFail p input =
      case parse p "Example" input of
        Left{}  -> "no parse"
        Right _ -> error "Parsed but shouldn't"
