{-# LANGUAGE RecordWildCards #-}

module Utils.Range (Range (..), Ranged (..), Position (..), rangeLen, toSourcePos) where

import Relude
import Text.Megaparsec (SourcePos (SourcePos), mkPos)
import Text.Show qualified

data Range = Range {start :: !Position, end :: !Position}
    deriving (Show, Eq)

rangeLen :: Range -> Int
rangeLen (Range{start, end}) = offset end - offset start

data Ranged a = Ranged {contents :: a, range :: Range}

instance (Eq a) => Eq (Ranged a) where
    (==) a b = (contents a) == (contents b) && (range a) == (range b)

instance (Ord a) => Ord (Ranged a) where
    compare = comparing contents

data Position = Position {filePath :: FilePath, offset :: !Int, line :: !Int, column :: !Int}
    deriving (Show, Eq, Ord)

toSourcePos :: Position -> SourcePos
toSourcePos (Position{filePath, line, column, ..}) = SourcePos filePath (mkPos line) (mkPos column)

instance Semigroup Range where
    (<>) a b = Range{start = start a `min` start b, end = end a `max` end b}

instance Functor Ranged where
    fmap f (Ranged{contents, range}) = Ranged{contents = f contents, range}

instance (Show a) => Show (Ranged a) where
    show (Ranged{contents, range}) = show contents ++ " <" ++ show range ++ ">"
