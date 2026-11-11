{-# LANGUAGE RecordWildCards #-}

module Utils.Range (Range (..), Ranged (..), Position (..), rangeLen, toSourcePos, fromSourcePos) where

import Relude
import Text.Megaparsec (SourcePos (SourcePos, sourceColumn, sourceLine, sourceName), mkPos)
import Text.Megaparsec.Pos (unPos)
import Text.Show qualified

data Range = Range {rangeStart :: !Position, rangeEnd :: !Position}
    deriving (Show, Eq)

rangeLen :: Range -> Int
rangeLen (Range{rangeStart, rangeEnd}) = posOffset rangeEnd - posOffset rangeStart

-- I hate this naming convention so much but it high-key works here
data Ranged a = Ranged {unRanged :: a, range :: Range}

instance (Eq a) => Eq (Ranged a) where
    (==) a b = (unRanged a) == (unRanged b) && (range a) == (range b)

instance (Ord a) => Ord (Ranged a) where
    compare = comparing unRanged

data Position = Position {posOffset :: !Int, posLine :: !Int, posColumn :: !Int}
    deriving (Show, Eq, Ord)

toSourcePos :: FilePath -> Position -> SourcePos
toSourcePos filePath (Position{posLine, posColumn}) = SourcePos filePath (mkPos posLine) (mkPos posColumn)

-- | **LOSSY CONVERSION**
fromSourcePos :: Int -> SourcePos -> Position
fromSourcePos posOffset (SourcePos{sourceLine, sourceColumn, ..}) = Position{posOffset, posLine = unPos sourceLine, posColumn = unPos sourceColumn}

instance Semigroup Range where
    (<>) a b = Range{rangeStart = rangeStart a `min` rangeStart b, rangeEnd = rangeEnd a `max` rangeEnd b}

instance Functor Ranged where
    fmap f (Ranged{unRanged, range}) = Ranged{unRanged = f unRanged, range}

instance (Show a) => Show (Ranged a) where
    show (Ranged{unRanged, range}) = show unRanged ++ " <" ++ show range ++ ">"
