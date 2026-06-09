{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Safe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.Pos
-- Copyright   :  (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  derek.a.elkins@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Textual source positions.
--
-----------------------------------------------------------------------------

module Text.Parsec.Pos
    ( SourceName, Line, Column
    , SourcePos
    , sourceLine, sourceColumn, sourceName, sourceOffset
    , incSourceLine, incSourceColumn, incSourceOffset
    , setSourceLine, setSourceColumn, setSourceOffset, setSourceName
    , newPos, initialPos
    , updatePosChar, updatePosString
    ) where

import Data.Data (Data)
import Data.Typeable (Typeable)

-- < Source positions: a file name, a line and a column
-- upper left is (1,1)

type SourceName = String
type Line       = Int
type Column     = Int
type Offset     = Int

-- | The abstract data type @SourcePos@ represents source positions. It
-- contains the name of the source (i.e. file name), a line number,
-- a column number, and a character offset.
-- @SourcePos@ is an instance of the 'Show', 'Eq' and 'Ord' class.

data SourcePos  = SourcePos SourceName !Line !Column !Offset
    deriving ( Eq, Ord, Data, Typeable)

-- | Create a new 'SourcePos' with the given source name,
-- line number and column number.

newPos :: SourceName -> Line -> Column -> Offset -> SourcePos
newPos name line column offset
    = SourcePos name line column offset

-- | Create a new 'SourcePos' with the given source name,
-- and line number and column number set to 1, the upper left.

initialPos :: SourceName -> SourcePos
initialPos name
    = newPos name 1 1 0

-- | Extracts the name of the source from a source position.

sourceName :: SourcePos -> SourceName
sourceName (SourcePos name _line _column _offset) = name

-- | Extracts the line number from a source position.

sourceLine :: SourcePos -> Line
sourceLine (SourcePos _name line _column _offset) = line

-- | Extracts the column number from a source position.

sourceColumn :: SourcePos -> Column
sourceColumn (SourcePos _name _line column _offset) = column

-- | Extracts the offset from a source position.

sourceColumn :: SourcePos -> Column
sourceColumn (SourcePos _name _line _column offset) = offset

-- | Increments the line number of a source position.

incSourceLine :: SourcePos -> Line -> SourcePos
incSourceLine (SourcePos name line column offset) n = SourcePos name (line+n) column offset

-- | Increments the column number of a source position.

incSourceColumn :: SourcePos -> Column -> SourcePos
incSourceColumn (SourcePos name line column offset) n = SourcePos name line (column+n) offset

-- | Increments the offset of a source position.

incSourceOffset :: SourcePos -> Column -> SourcePos
incSourceOffset (SourcePos name line column offset) n = SourcePos name line column (offset+n)

-- | Set the name of the source.

setSourceName :: SourcePos -> SourceName -> SourcePos
setSourceName (SourcePos _name line column offset) n = SourcePos n line column offset

-- | Set the line number of a source position.

setSourceLine :: SourcePos -> Line -> SourcePos
setSourceLine (SourcePos name _line column offset) n = SourcePos name n column offset

-- | Set the column number of a source position.

setSourceColumn :: SourcePos -> Column -> SourcePos
setSourceColumn (SourcePos name line _column offset) n = SourcePos name line n offset

-- | Set the offset of a source position.

setSourceColumn :: SourcePos -> Column -> SourcePos
setSourceColumn (SourcePos name line column _offset) n = SourcePos name line column n

-- | The expression @updatePosString pos s@ updates the source position
-- @pos@ by calling 'updatePosChar' on every character in @s@, ie.
-- @foldl updatePosChar pos string@.

updatePosString :: SourcePos -> String -> SourcePos
updatePosString pos string
    = foldl updatePosChar pos string

-- | Update a source position given a character. If the character is a
-- newline (\'\\n\') or carriage return (\'\\r\') the line number is
-- incremented by 1. If the character is a tab (\'\t\') the column
-- number is incremented to the nearest 8'th column, ie. @column + 8 -
-- ((column-1) \`mod\` 8)@. In all other cases, the column is
-- incremented by 1.

updatePosChar   :: SourcePos -> Char -> SourcePos
updatePosChar (SourcePos name line column offset) c
    = case c of
        '\n' -> SourcePos name (line+1) 1 (offset+1)
        '\t' -> SourcePos name line (column + 8 - ((column-1) `mod` 8)) (offset+1)
        _    -> SourcePos name line (column + 1) (offset + 1)

instance Show SourcePos where
  show (SourcePos name line column)
    | null name = showLineColumnOffset
    | otherwise = "\"" ++ name ++ "\" " ++ showLineColumn
    where
      showLineColumnOffset = "(line " ++ show line ++
                             ", column " ++ show column ++
                             ", offset " ++ show offset ++
                             ")"
