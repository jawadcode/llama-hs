module Utils.Range (Range (..), Ranged (..), Position (..)) where

import Relude
import Text.Show qualified

data Range = Range {start :: !Position, end :: !Position}
  deriving (Show)

data Ranged a = Ranged {contents :: a, range :: Range}

data Position = Position {offset :: !Int, line :: !Int, column :: !Int}
  deriving (Show, Eq, Ord)

instance Semigroup Range where
  (<>) a b = Range {start = start a `min` start b, end = end a `max` end b}

instance Functor Ranged where
  fmap f (Ranged {contents, range}) = Ranged {contents = f contents, range}

instance (Show a) => Show (Ranged a) where
  show (Ranged {contents, range}) = show contents ++ " <" ++ show range ++ ">"
