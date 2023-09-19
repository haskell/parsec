{-# LANGUAGE BangPatterns #-}
module Bugs.Bug179
       ( tests
       ) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Functor
import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit

import qualified Control.Applicative
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Text.Parsec.Pos as P

tests :: TestTree
tests = testGroup "many try (#179)"
    [ testCase "Parsec" $ examples parser3
    , testCase "manyDefault" $ examples parser1
    , testCase "C.Applicative" $ examples parser2
    ]
 where
    examples p = do
        res1 <- parseString p $ "foo#bar"
        res1 @?= ["foo", "#", "bar"]


    parseString :: P.Parser [String] -> String -> IO [String]
    parseString p input =
      case P.parse p "" input of
        Left err -> assertFailure $ show err
        Right xs -> return xs

identifier :: P.Parser String
identifier = mfilter (not . null) (scan (\s c -> if isAlphaNum c then Just s else Nothing) ())

parser1 :: P.Parser [String]
parser1 = manyDefault (P.try identifier <|> hash)

parser2 :: P.Parser [String]
parser2 = Control.Applicative.many (P.try identifier <|> hash)

parser3 :: P.Parser [String]
parser3 = P.many (P.try identifier <|> hash)

hash :: P.Parser String
hash = "#" <$ P.char '#'

-- many's default definition
manyDefault :: Alternative f => f a -> f [a]
manyDefault v = many_v
    where
    many_v = some_v <|> pure []
    some_v = liftA2 (:) v many_v

-- | Scan the input text, accumulating characters as long as the scanning
-- function returns true.
scan :: (s -> Char -> Maybe s) -- ^ scan function
     -> s                      -- ^ initial state
     -> P.Parser String
scan f st = do
  s@P.State{ P.stateInput = inp, P.statePos = pos } <- P.getParserState
  go inp st pos 0 $ \inp' pos' n -> do
    let s' = s{ P.stateInput = inp', P.statePos = pos' }
    P.setParserState s' $> take n inp
  where
    go inp s !pos !n cont
      = case inp of
          [] -> cont inp pos n        -- ran out of input
          c : inp' ->
             case f s c of
               Nothing -> cont inp pos n   -- scan function failed
               Just s' -> go inp' s' (P.updatePosChar pos c) (n+1) cont
