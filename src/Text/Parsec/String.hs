{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.String
-- Copyright   :  (c) Paolo Martini 2007
-- License     :  BSD-style (see the file libraries/parsec/LICENSE)
--
-- Maintainer  :  derek.a.elkins@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Make Strings an instance of 'Stream' with 'Char' token type.
--
-----------------------------------------------------------------------------

module Text.Parsec.String
    ( Parser, ParserU, GenParser, parseFromFile, parseFromFile'
    ) where

#if __GLASGOW_HASKELL__ < 710
import Data.Functor((<$>))
#endif

import Text.Parsec.Error
import Text.Parsec.Prim

type ParserU u = Parsec String u
type Parser = ParserU ()
type GenParser tok st = Parsec [tok] st


-- | @parseFromFile' p u filePath@ runs a strict bytestring parser @p@ on the
-- input read from @filePath@ using 'Prelude.readFile' with start state @u@.
-- Returns either a 'ParseError' ('Left') or a value of type @a@ ('Right').
--
-- >  main    = do{ result <- parseFromFile' numbers () "digits.txt"
-- >              ; case result of
-- >                  Left err  -> print err
-- >                  Right xs  -> print (sum xs)
-- >              }
parseFromFile' :: ParserU u a -> u -> FilePath -> IO (Either ParseError a)
parseFromFile' p u fname = runP p u fname <$> readFile fname


-- | @parseFromFile p filePath@ runs a strict bytestring parser @p@ on the
-- input read from @filePath@ using 'Prelude.readFile'. Returns either a 'ParseError'
-- ('Left') or a value of type @a@ ('Right').
--
-- >  main    = do{ result <- parseFromFile numbers "digits.txt"
-- >              ; case result of
-- >                  Left err  -> print err
-- >                  Right xs  -> print (sum xs)
-- >              }
parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
parseFromFile = (`parseFromFile'` ())
