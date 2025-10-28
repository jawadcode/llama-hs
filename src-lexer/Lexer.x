{

module Lexer
  ( Token (..),
    AlexPosn (..),
    alexScanTokens,
    getPosn,
    readLine
  ) where

import Data.Maybe (fromJust)

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS (head, pack, readInt)

}

%wrapper "posn-bytestring"

$digit = 0-9      -- digits
$alpha = [a-zA-Z] -- alphabetic characters
@ident = $alpha [$alpha $digit \_ \']*

tokens :-
  $white+          ;
  "--".*           ;
  let              { tok (\p _ -> Let p) }
  in               { tok (\p _ -> In p) }
  $digit+          { tok (\p s -> Int p (fst $ fromJust $ BS.readInt s)) }
  [\=\+\-\*\/\(\)] { tok (\p s -> Sym p (BS.head s)) }
  @ident           { tok (\p s -> Var p s) }

{

-- Each right-hand side has type :: AlexPosn -> ByteString -> Token

-- Some action helpers:
tok f p s = f p s

-- The token type:
data Token
  = Let AlexPosn
  | In  AlexPosn
  | Sym AlexPosn Char
  | Var AlexPosn ByteString
  | Int AlexPosn Int
  deriving (Eq,Show)

getPosn :: Token -> AlexPosn
getPosn (Let p) = p
getPosn (In p) = p
getPosn (Sym p _) = p
getPosn (Var p _) = p
getPosn (Int p _) = p

readLine :: IO ByteString
readLine = BS.pack <$> getLine

}
