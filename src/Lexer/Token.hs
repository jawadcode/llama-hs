module Lexer.Token (Token (..), RangedToken) where

import Relude
import Utils.Range (Ranged)

data Token
  = Let
  | In
  | Equals
  | Plus
  | Minus
  | Multiply
  | Divide
  | Ident ByteString
  | IntLit Int
  | EOF
  deriving (Eq, Show)

type RangedToken = Ranged Token
