{
{-# LANGUAGE ImplicitPrelude #-}

module Lexer.Internal.Alex ( AlexPosn (..), AlexInput, Alex, AlexUserState (..),
    alexGetUserState, alexSetUserState, alexGetInput, skip, alexSetStartCode,
    alexMove, runAlex, alexMonadScan ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS (init)
import Lexer.Token (Token (..))
import Lexer.Internal.Helpers (alexInitUserState, enterNewComment, embedComment,
    unembedComment, tok, tokIdent, tokInt, tokFloat, tokStr, alexEOF)
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
<0>              fun      { tok Fun }
<0>              const    { tok Const }
<0>              let      { tok Let }
<0>              fn       { tok Fn }
<0>              if       { tok If }
<0>              then     { tok Then }
<0>              else     { tok Else }
<0>              cond     { tok Cond }
<0>              match    { tok Match }
<0>              do       { tok Do }
<0>              end      { tok End }

<0>              @ident   { tokIdent }
<0>              unit     { tok UnitLit }
<0>              true     { tok TokTrue }
<0>              false    { tok TokFalse }
<0>              $digit+  { tokInt }
<0>              @str     { tokStr }
<0>              @float   { tokFloat }

<0>              "+"      { tok IAdd }
<0>              "-"      { tok ISub }
<0>              "*"      { tok IMul }
<0>              "/"      { tok IDiv }


<0>              "+"      { tok IAdd }
<0>              "-"      { tok ISub }
<0>              "*"      { tok IMul }
<0>              "/"      { tok IDiv }
<0>              "+."     { tok FAdd }
<0>              "-."     { tok FSub }
<0>              "*."     { tok FMul }
<0>              "/."     { tok FDiv }
<0>              "%"      { tok Mod }
<0>              "<"      { tok LessThan }
<0>              "<="     { tok LessOrEq }
<0>              ">"      { tok GreaterThan }
<0>              ">="     { tok GreaterOrEq }
<0>              "=="     { tok Eq }
<0>              "!="     { tok NotEq }
<0>              "!"      { tok Not }
<0>              "and"    { tok And }
<0>              "or"     { tok Or }
<0>              "ref"    { tok Ref }
<0>              "^"      { tok Caret }
<0>              "("      { tok LParen }
<0>              ")"      { tok RParen }
<0>              "["      { tok LSquare }
<0>              "]"      { tok RSquare }
<0>              "="      { tok Bind }
<0>              ":="     { tok Becomes }
<0>              "|>"     { tok FnPipe }
<0>              "|"      { tok Pipe }
<0>              ":+"     { tok Append }
<0>              "++"     { tok Concat }
<0>              ","      { tok Comma }
<0>              "->"     { tok Arrow }
<0>              "=>"     { tok FatArrow }
<0>              ":"      { tok Colon }
<0>              ";"      { tok Semi }

{
data AlexUserState = AlexUserState {commentDepth :: Int64}
}
