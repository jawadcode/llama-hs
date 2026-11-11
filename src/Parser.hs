module Parser () where

import Relude

import Data.Set qualified as Set

import Lexer (LStream)
import Text.Megaparsec (Parsec, initialPos)
import Utils.Range (Range (..), Ranged (..), fromSourcePos)
