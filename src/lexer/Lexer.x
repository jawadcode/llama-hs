{
module Lexer
  ( Token (..),
    AlexPosn (..),
    scanMany,
    readLine,
  )
where

import Control.Monad (when)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS (foldl', pack, readInt, take)
import Data.Function ((&))
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Utils.Range (Position (..), Range (..), Ranged (..))
}

%wrapper "monadUserState-bytestring"

$digit = 0-9      -- digits
$alpha = [a-zA-Z] -- alphabetic characters
@ident = $alpha [$alpha $digit \_ \']*

tokens :-
<0>              $white+  ;
<0>              "(*"     { enterNewComment `andBegin` state_comment }
<0>              "(*"     { enterNewComment `andBegin` state_comment }
<state_comment>  "(*"     { embedComment }
<state_comment>  "*)"     { unembedComment }
<state_comment>  .        ;
<state_comment>  \n       { skip }
<0>              let      { tok Let }
<0>              in       { tok In }
<0>              "="      { tok Equals }
<0>              "+"      { tok Plus }
<0>              "-"      { tok Minus }
<0>              "*"      { tok Multiply }
<0>              "/"      { tok Divide }
<0>              $digit+  { tokInt }
<0>              @ident   { tokIdent }

{
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

data AlexUserState = AlexUserState {commentDepth :: Int}

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {commentDepth = 0}

getCommentDepth :: Alex Int
getCommentDepth = Alex $ \state@AlexState {alex_ust = ust} ->
  Right (state, commentDepth ust)

setCommentDepth :: Int -> Alex ()
setCommentDepth depth = Alex $ \state -> Right (state {alex_ust = (alex_ust state) {commentDepth = depth}}, ())

type Action result = AlexInput -> Int64 -> Alex result

initialState :: Int
initialState = 0

alexEOF :: Alex RangedToken
alexEOF = do
  (position, _, _, _) <- alexGetInput
  let pos = convertPos position
  pure Ranged {contents = EOF, range = Range {start = pos, end = pos}}

enterNewComment, embedComment, unembedComment :: Action RangedToken
enterNewComment input len = do
  setCommentDepth 1
  skip input len
embedComment input len = do
  depth <- getCommentDepth
  setCommentDepth (depth + 1)
  skip input len
unembedComment input len = do
  depth <- getCommentDepth
  setCommentDepth (depth - 1)
  when (depth == 1) (alexSetStartCode initialState)
  skip input len

tok :: Token -> Action RangedToken
tok tk input len = pure Ranged {contents = tk, range = getRange input len}

tokInt, tokIdent :: Action RangedToken
tokInt input@(_, _, string, _) len =
  pure
    Ranged
      { contents = BS.take len string & BS.readInt & fromJust & fst & IntLit,
        range = getRange input len
      }
tokIdent input@(_, _, string, _) len =
  pure
    Ranged
      { contents = Ident $ BS.take len string,
        range = getRange input len
      }

getRange :: AlexInput -> Int64 -> Range
getRange (start', _, string, _) len = Range {start, end}
  where
    start = convertPos start'
    end = BS.take len string & BS.foldl' alexMove start' & convertPos

convertPos :: AlexPosn -> Position
convertPos (AlexPn offset line column) = Position {offset, line, column}

scanMany :: ByteString -> Either String [RangedToken]
scanMany input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if contents output == EOF
        then pure [output]
        else (output :) <$> go

readLine :: IO ByteString
readLine = BS.pack <$> getLine
}
