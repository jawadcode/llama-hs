{
{-# LANGUAGE ImplicitPrelude #-}

module Lexer.Internal.Alex ( AlexPosn (..), AlexInput, Alex, AlexUserState (..),
  alexGetUserState, alexSetUserState, alexGetInput, skip, alexSetStartCode,
  alexMove, runAlex, alexMonadScan ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS (init)
import Lexer.Token (Token (..))
import Lexer.Internal.Helpers (alexInitUserState, enterNewComment, embedComment, unembedComment, tok, tokIdent, tokInt, tokFloat, tokStr, alexEOF)
}

%wrapper "monadUserState-bytestring"

$digit = 0-9      -- digits
$alpha = [a-zA-Z] -- alphabetic characters
@ident = $alpha [$alpha $digit \_ \']*
@float = (($digit+ (\. $digit+)?) | (\. $digit+))([Ee](\+ | \-)? $digit+)?
@str = \" ([^\"\\] | \\ [\s\S])* \"

tokens :-
<0>              $white+  ;
<0>              "(*"     { enterNewComment `andBegin` state_comment }
<0>              "(*"     { enterNewComment `andBegin` state_comment }
<state_comment>  "(*"     { embedComment }
<state_comment>  "*)"     { unembedComment }
<state_comment>  .        ;
<state_comment>  \n       { skip }
<0>              let      { tok Let }

<0>              "unit"   { tok UnitLit }
<0>              @ident   { tokIdent }
<0>              $digit+  { tokInt }
<0>              @float   { tokFloat }
<0>              @str     { tokStr }

<0>              "+"      { tok IAdd }
<0>              "-"      { tok ISub }
<0>              "*"      { tok IMul }
<0>              "/"      { tok IDiv }

{
data AlexUserState = AlexUserState {commentDepth :: Int64}
}
