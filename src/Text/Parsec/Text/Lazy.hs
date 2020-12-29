{-# LANGUAGE CPP #-}
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
    ( Parser, ParserU, GenParser, parseFromFile, parseFromFile'
    ) where

#if __GLASGOW_HASKELL__ < 710
import Data.Functor((<$>))
#endif

import qualified Data.Text.Lazy as Text
import Text.Parsec.Prim
import Text.Parsec.Error
import Data.Text.Lazy.IO as TL

type ParserU u = Parsec Text.Text u
type Parser = ParserU ()
type GenParser st = Parsec Text.Text st


-- | @parseFromFile' p u filePath@ runs a strict bytestring parser @p@ on the
-- input read from @filePath@ using 'Data.Text.Lazy.IO.readFile' with start state @u@.
-- Returns either a 'ParseError' ('Left') or a value of type @a@ ('Right').
--
-- >  main    = do{ result <- parseFromFile' numbers () "digits.txt"
-- >              ; case result of
-- >                  Left err  -> print err
-- >                  Right xs  -> print (sum xs)
-- >              }
parseFromFile' :: ParserU u a -> u -> FilePath -> IO (Either ParseError a)
parseFromFile' p u fname = runP p u fname <$> TL.readFile fname


-- | @parseFromFile p filePath@ runs a strict bytestring parser @p@ on the
-- input read from @filePath@ using 'Data.Text.Lazy.IO.readFile'. Returns either a 'ParseError'
-- ('Left') or a value of type @a@ ('Right').
--
-- >  main    = do{ result <- parseFromFile numbers "digits.txt"
-- >              ; case result of
-- >                  Left err  -> print err
-- >                  Right xs  -> print (sum xs)
-- >              }
parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
parseFromFile = (`parseFromFile'` ())
