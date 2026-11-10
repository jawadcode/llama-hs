{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Parser () where

import Data.ByteString.Lazy.Char8 qualified as BS (ByteString, append, drop, pack, splitAt, takeWhile, unpack)
import Data.List qualified as DL
import Lexer.Token (RangedToken)
import Relude
import Text.Megaparsec
import Utils.Range qualified as Range

{- Custom Token Stream Type -}

data LStream = LStream
    { streamInput :: BS.ByteString
    , stream :: [RangedToken]
    }

instance Stream LStream where
    type Token LStream = RangedToken
    type Tokens LStream = [RangedToken]

    tokenToChunk Proxy token = [token]
    tokensToChunk Proxy tokens = tokens
    chunkToTokens Proxy = id
    chunkLength Proxy = length
    chunkEmpty Proxy = null
    take1_ (LStream _ []) = Nothing
    take1_ (LStream source (token : tokens)) =
        Just
            ( token
            , LStream (BS.drop (toEnum (tokensLength (Proxy @LStream) (token :| []))) source) tokens
            )
    takeN_ n (LStream source tokens)
        | n <= 0 = Just ([], LStream source tokens)
        | null tokens = Nothing
        | otherwise =
            let (before, after) = splitAt n tokens
             in case nonEmpty before of
                    Nothing -> Just (before, LStream source after)
                    Just token -> Just (before, LStream (BS.drop (toEnum (tokensLength (Proxy @LStream) token)) source) after)

instance VisualStream LStream where
    showTokens Proxy = intercalate " " . toList . fmap (show . Range.contents)
    tokensLength Proxy tokens = sum (tokens <&> (Range.range >>> Range.rangeLen))

instance TraversableStream LStream where
    reachOffset o PosState{..} =
        ( Just (prefix ++ BS.unpack restOfLine)
        , PosState
            { pstateInput = LStream{streamInput = postStr, stream = post}
            , pstateOffset = max pstateOffset o
            , pstateSourcePos = newSourcePos
            , pstateTabWidth
            , pstateLinePrefix = prefix
            }
        )
      where
        prefix = if sameLine then pstateLinePrefix ++ preLine else preLine
        sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
        newSourcePos =
            case post of
                [] -> case stream pstateInput of
                    [] -> pstateSourcePos
                    xs -> Range.toSourcePos $ Range.end $ Range.range $ DL.last xs
                (x : _) -> Range.toSourcePos $ Range.start $ Range.range x
        (pre, post) = splitAt (o - pstateOffset) (stream pstateInput)
        (preStr, postStr) = BS.splitAt (toEnum tokensConsumed) (streamInput pstateInput)
        preLine = reverse . takeWhile (/= '\n') . reverse $ BS.unpack $ preStr
        tokensConsumed =
            case nonEmpty pre of
                Nothing -> 0
                Just nePre -> tokensLength (Proxy @LStream) nePre
        restOfLine = BS.takeWhile (/= '\n') postStr
