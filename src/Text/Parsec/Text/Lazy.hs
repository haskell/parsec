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
-- Convenience definitions for working with lazy 'Text.Text'.
--
-----------------------------------------------------------------------------

module Text.Parsec.Text.Lazy
    ( Parser, GenParser, parseFromFile
    ) where

import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as TL

import Text.Parsec.Prim
import Text.Parsec.Error

type Parser = Parsec Text.Text ()
type GenParser st = Parsec Text.Text st

-- | @parseFromFile p filePath@ runs a strict text parser @p@ on the
-- input read from @filePath@ using 'Data.Text.Lazy.IO.readFile'. Returns either a 'ParseError'
-- ('Left') or a value of type @a@ ('Right').
--
-- >  main    = do{ result <- parseFromFile numbers "digits.txt"
-- >              ; case result of
-- >                  Left err  -> print err
-- >                  Right xs  -> print (sum xs)
-- >              }
--
-- @since 3.1.14.0

parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
parseFromFile p fname
    = do input <- TL.readFile fname
         return (runP p () fname input)
