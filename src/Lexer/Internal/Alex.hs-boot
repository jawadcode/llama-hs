{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE KindSignatures #-}

module Lexer.Internal.Alex where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Int (Int64)
import Data.Kind (Type)

import Lexer.Token (RangedToken)

data AlexPosn = AlexPn !Int !Int !Int
type AlexInput = (AlexPosn, Char, ByteString, Int64)

data Alex (a :: Type)
instance Functor Alex
instance Applicative Alex
instance Monad Alex

data AlexUserState = AlexUserState {commentDepth :: Int64}

alexGetUserState :: Alex AlexUserState
alexSetUserState :: AlexUserState -> Alex ()

alexGetInput :: Alex AlexInput

skip :: AlexInput -> Int64 -> Alex RangedToken

alexSetStartCode :: Int -> Alex ()

alexMonadScan :: Alex RangedToken

alexMove :: AlexPosn -> Char -> AlexPosn

runAlex :: ByteString -> Alex a -> Either String a
