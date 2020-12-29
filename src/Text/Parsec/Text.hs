{-# LANGUAGE Safe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.String
-- Copyright   :  (c) Antoine Latter 2011
-- License     :  BSD-style (see the file libraries/parsec/LICENSE)
--
-- Maintainer  :  aslatter@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Convinience definitions for working with 'Text.Text'.
--
-----------------------------------------------------------------------------

module Text.Parsec.Text
    ( Parser, ParserU, GenParser, parseFromFile, parseFromFile'
    ) where

import qualified Data.Text as Text
import Text.Parsec.Prim
import Text.Parsec.Error
import Data.Text.IO as T

type ParserU u = Parsec Text.Text u
type Parser = ParserU ()
type GenParser st = Parsec Text.Text st


-- | @parseFromFile' p u filePath@ runs a strict bytestring parser @p@ on the
-- input read from @filePath@ using 'Data.Text.IO.readFile' with start state @u@.
-- Returns either a 'ParseError' ('Left') or a value of type @a@ ('Right').
--
-- >  main    = do{ result <- parseFromFile' numbers () "digits.txt"
-- >              ; case result of
-- >                  Left err  -> print err
-- >                  Right xs  -> print (sum xs)
-- >              }
parseFromFile' :: ParserU u a -> u -> FilePath -> IO (Either ParseError a)
parseFromFile' p u fname = runP p u fname <$> T.readFile fname


-- | @parseFromFile p filePath@ runs a strict bytestring parser @p@ on the
-- input read from @filePath@ using 'Data.Text.IO.readFile'. Returns either a 'ParseError'
-- ('Left') or a value of type @a@ ('Right').
--
-- >  main    = do{ result <- parseFromFile numbers "digits.txt"
-- >              ; case result of
-- >                  Left err  -> print err
-- >                  Right xs  -> print (sum xs)
-- >              }
parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
parseFromFile = (`parseFromFile'` ())
