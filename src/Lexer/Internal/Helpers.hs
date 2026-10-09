module Lexer.Internal.Helpers where

import Data.ByteString.Lazy.Char8 qualified as BS (ByteString, foldl', init, readInt, tail, take, unpack)
import Data.Maybe (fromJust)
import {-# SOURCE #-} Lexer.Internal.Alex (Alex, AlexInput, AlexPosn (..), AlexUserState (..), alexGetInput, alexGetUserState, alexMonadScan, alexMove, alexSetStartCode, alexSetUserState, runAlex, skip)
import Lexer.Token (RangedToken, Token (..))
import Numeric (readFloat)
import Relude
import Utils.Range (Position (..), Range (..), Ranged (..))

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState{commentDepth = 0}

getCommentDepth :: Alex Int64
getCommentDepth = alexGetUserState <&> commentDepth

setCommentDepth :: Int64 -> Alex ()
setCommentDepth depth = do
    ust <- alexGetUserState
    alexSetUserState ust{commentDepth = depth}

type Action result = AlexInput -> Int64 -> Alex result

initialState :: Int
initialState = 0

alexEOF :: Alex RangedToken
alexEOF = do
    (position, _, _, _) <- alexGetInput
    let pos = convertPos position
    pure Ranged{contents = EOF, range = Range{start = pos, end = pos}}

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
tok tk input len = pure Ranged{contents = tk, range = getRange input len}

tokIdent, tokInt, tokFloat, tokStr :: Action RangedToken
tokIdent = tokHelper (decodeUtf8 >>> Ident)
tokInt = tokHelper (BS.readInt >>> fromJust >>> fst >>> IntLit)
tokFloat = tokHelper (BS.unpack >>> readFloat >>> listToMaybe >>> fromJust >>> fst >>> FloatLit)
tokStr = tokHelper (BS.tail >>> BS.init >>> decodeUtf8 >>> StrLit)

tokHelper :: (BS.ByteString -> Token) -> Action RangedToken
tokHelper f input@(_, _, string, _) len =
    pure Ranged{contents = BS.take len string & f, range = getRange input len}

getRange :: AlexInput -> Int64 -> Range
getRange (start', _, string, _) len = Range{start, end}
  where
    start = convertPos start'
    end = BS.take len string & BS.foldl' alexMove start' & convertPos

convertPos :: AlexPosn -> Position
convertPos (AlexPn offset line column) = Position{offset, line, column}

scanMany :: BS.ByteString -> Either String [RangedToken]
scanMany input = runAlex input go
  where
    go = do
        output <- alexMonadScan
        if contents output == EOF
            then pure [output]
            else (output :) <$> go
