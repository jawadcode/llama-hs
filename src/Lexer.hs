{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Lexer (scanMany, LStream) where

import Relude

import Data.ByteString.Lazy.Char8 qualified as BS (ByteString, drop, splitAt, takeWhile, unpack)
import Data.List qualified as DL
import Lexer.Internal.Helpers qualified as L (scanMany)
import Lexer.Token (RangedToken)
import Text.Megaparsec qualified as TM (PosState (..), SourcePos (sourceName), Stream (..), TraversableStream (..), VisualStream (..), sourceLine)
import Utils.Range (Range (..), Ranged (..), rangeLen, toSourcePos)

scanMany :: BS.ByteString -> Either String [RangedToken]
scanMany = L.scanMany

{- Custom Token Stream Type -}

data LStream = LStream
    { streamInput :: BS.ByteString
    , stream :: [RangedToken]
    }

instance TM.Stream LStream where
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
            , LStream (BS.drop (toEnum (TM.tokensLength proxy (token :| []))) source) tokens
            )
    takeN_ n (LStream source tokens)
        | n <= 0 = Just ([], LStream source tokens)
        | null tokens = Nothing
        | otherwise =
            let (before, after) = splitAt n tokens
             in case nonEmpty before of
                    Nothing -> Just (before, LStream source after)
                    Just token -> Just (before, LStream ((TM.tokensLength proxy token & toEnum & BS.drop) source) after)

    takeWhile_ f (LStream source tokens) =
        let (matches, rest) = span f tokens
         in case nonEmpty matches of
                Nothing -> (matches, LStream source rest)
                Just mat -> (matches, LStream ((TM.tokensLength proxy mat & toEnum & BS.drop) source) rest)

instance TM.VisualStream LStream where
    showTokens Proxy = fmap (unRanged >>> show) >>> toList >>> intercalate " "
    tokensLength Proxy tokens = sum (tokens <&> (range >>> rangeLen))

instance TM.TraversableStream LStream where
    reachOffset o (TM.PosState{pstateInput, pstateOffset, pstateSourcePos, pstateTabWidth, pstateLinePrefix}) =
        ( Just (prefix ++ BS.unpack restOfLine)
        , TM.PosState
            { pstateInput = LStream{streamInput = postStr, stream = post}
            , pstateOffset = max pstateOffset o
            , pstateSourcePos = newSourcePos
            , pstateTabWidth
            , pstateLinePrefix = prefix
            }
        )
      where
        prefix = if sameLine then pstateLinePrefix ++ preLine else preLine
        sameLine = TM.sourceLine newSourcePos == TM.sourceLine pstateSourcePos
        newSourcePos =
            case post of
                [] -> case stream pstateInput of
                    [] -> pstateSourcePos
                    tokens -> DL.last tokens & range & rangeEnd & toSourcePos filePath
                (token : _) -> range token & rangeStart & toSourcePos filePath
        filePath = TM.sourceName pstateSourcePos
        (pre, post) = splitAt (o - pstateOffset) (stream pstateInput)
        (preStr, postStr) = BS.splitAt (toEnum tokensConsumed) (streamInput pstateInput)
        preLine = preStr & BS.unpack & (reverse >>> takeWhile (/= '\n') >>> reverse)
        tokensConsumed =
            case nonEmpty pre of
                Nothing -> 0
                Just nePre -> TM.tokensLength proxy nePre
        restOfLine = BS.takeWhile (/= '\n') postStr

proxy :: Proxy LStream
proxy = Proxy
