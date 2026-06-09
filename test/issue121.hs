module Main (main) where

import Data.Map (fromList)
import Text.Parsec

sepBy1Try p sep = do
  x <- p
  xs <- many (try $ sep *> p)
  return (x : xs)

key =
  try (string "byr")
    <|> try (string "cid")
    <|> try (string "eyr")
    <|> try (string "ecl")
    <|> try (string "hgt")
    <|> try (string "hcl")
    <|> try (string "iyr")
    <|> try (string "pid")

passportKV = try $ do
  k <- key
  char ':'
  v <- many1 (try alphaNum <|> try (char '#'))
  return (k, v)

passportLine = sepBy1Try passportKV space

passport = do
  lines <- sepBy1Try passportLine endOfLine
  return $ fromList [kv | line <- lines, kv <- line]

passports = sepBy1 passport (try (endOfLine *> endOfLine *> pure ()) <|> (endOfLine *> eof *> pure ()))

main = do
  let raw = "byr:2001 iyr:2011\necl:brn\npid:487702556 hcl:#602927\nhgt:167cm eyr:2026\n"
  print $ parse passports "(poop)" raw
