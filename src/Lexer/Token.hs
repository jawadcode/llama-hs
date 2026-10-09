module Lexer.Token (Token (..), RangedToken) where

import Relude
import Utils.Range (Ranged)

data Token
    = {- Keywords -}
      Fun
    | Const
    | Let
    | Fn
    | If
    | Then
    | Else
    | Cond
    | Match
    | Do
    | End
    | {- Literals -}
      UnitLit
    | True
    | False
    | Ident Text
    | IntLit Int
    | FloatLit Float
    | StrLit Text
    | {- Operator -}
      -- Arithmetic
      IAdd
    | ISub
    | IMul
    | IDiv
    | FAdd
    | FSub
    | FMul
    | FDiv
    | Mod
    | -- Comparison
      LessThan
    | LessOrEq
    | GreaterThan
    | GreaterOrEq
    | Eq
    | NotEq
    | -- Boolean
      Not
    | And
    | Or
    | -- Misc
      Ref
    | Caret
    | {- Delimeters -}
      LParen
    | RParen
    | LSquare
    | RSquare
    | {- Misc -}
      Bind
    | Becomes
    | FnPipe
    | Pipe
    | Append
    | Concat
    | Comma
    | Arrow
    | FatArrow
    | Colon
    | Semi
    | EOF
    deriving (Eq, Show)

type RangedToken = Ranged Token
