{

module Lexer
  ( Alex,
    AlexPosn (..),
    alexGetInput,
    alexError,
    runAlex,
    alexMonadScan,
    RangedToken,
    Token (..),
    runLex,
  )
where

import Control.Monad (when)

import Utils.Range (Position (..), Range (..), Ranged (..))

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
  ( foldl',
    getContents,
    pack,
    take,
    unpack,
  )

}

%wrapper "monadUserState-bytestring"

$digit = 0-9
$alpha = [a-zA-Z]
@id = ($alpha | \_) ($alpha | $digit | \_ | \' | \?)*

tokens :-
<0>       $white+      { skip } -- Using `skip` just to be explicit
<0>       "(*"         { nestComment `andBegin` comment }
<0>       "*)"         { \_ _ -> alexError "Error: Unexpected closing comment" }
<comment> "(*"         { nestComment }
<comment> "*)"         { deNestComment }
<comment> .            ;
<comment> \n           ;
<0>       let          { tok TokLet }
<0>       $digit+      { tokIntLit }
<0>       @id          { tokIdent }

{

data AlexUserState = AlexUserState { commentDepth :: Int }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { commentDepth = 0 }

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s { alex_ust = s' }, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f = Alex $ \s -> Right (s { alex_ust = f (alex_ust s) }, ())

nestComment :: AlexAction RangedToken
nestComment input len = do
  modify $ \s -> s { commentDepth = commentDepth s + 1 }
  skip input len

deNestComment :: AlexAction RangedToken
deNestComment input len = do
  state <- get
  let depth = commentDepth state - 1
  put state { commentDepth = depth }
  when (depth == 0) $
    alexSetStartCode 0
  skip input len

-- The token type:
data Token =
  -- Keywords:
    TokFun
  | TokConst
  | TokLet
  | TokFn
  | TokIf
  | TokThen
  | TokElse
  | TokCond
  | TokMatch
  | TokDo
  | TokEnd
  -- Literals:
  | TokIdent ByteString
  | TokUnitLit
  | TokTrue
  | TokFalse
  | TokIntLit Int
  | TokStringLit ByteString
  -- Special:
  | TokEOF
  deriving (Eq, Show)

type RangedToken = Ranged Token

convertPos (AlexPn offset line column) = Position {offset, line, column}

mkRange :: AlexInput -> Int64 -> Range
mkRange (start, _, string, _) len = Range {start = convertPos start, end = convertPos end}
  where
    end = BS.foldl' alexMove start $ BS.take len string

alexEOF :: Alex RangedToken
alexEOF = do
 startCode <- alexGetStartCode
 when (startCode == comment) $
   alexError "Error: Unexpected closing comment"
 (pos, _, _, _) <- alexGetInput
 let range = convertPos pos
 pure $ Ranged { contents = TokEOF, range = (Range { start = range, end = range }) }

tokIntLit input@(_, _, string, _) len =
  pure Ranged
    { contents = TokIntLit $ read $ BS.unpack string,
      range = mkRange input len
    }

tokIdent input@(_, _, string, _) len =
  pure Ranged
    { contents = TokIdent $ BS.take len string,
      range = mkRange input len
    }

tok :: Token -> AlexAction RangedToken
tok ctor input len =
  pure
    Ranged
      { contents = ctor,
        range = mkRange input len
      }

scanMany :: ByteString -> Either String [RangedToken]
scanMany input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if contents output == TokEOF
        then pure [output]
        else (output :) <$> go

runLex :: IO (Either String [RangedToken])
runLex = do
  line <- BS.pack <$> getLine
  pure $ scanMany line

}
