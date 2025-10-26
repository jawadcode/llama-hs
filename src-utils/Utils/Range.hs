module Utils.Range
  ( Range (..),
    Ranged (..),
    Position (..),
  )
where

import Text.Show qualified

data Range = Range {start :: !Position, end :: !Position}
  deriving (Show)

data Ranged a = Ranged {contents :: a, range :: Range}

data Position = Position {offset :: !Int, line :: !Int, column :: !Int}
  deriving (Show)

instance Semigroup Range where
  (<>) a b = Range {start = start a, end = end b}

instance Functor Ranged where
  fmap f (Ranged {contents, range}) = Ranged {contents = f contents, range}

-- FIXME: show is not a visible method of Show??
instance (Show a) => Show (Ranged a) where
  show (Ranged {contents, range}) = show contents ++ " <" ++ show range ++ ">"
