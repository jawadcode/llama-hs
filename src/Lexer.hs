module Lexer (scanMany) where

import Data.ByteString.Lazy.Char8 qualified as BS (ByteString)
import Lexer.Internal.Helpers qualified as L (scanMany)
import Lexer.Token (RangedToken)
import Relude

scanMany :: BS.ByteString -> Either String [RangedToken]
scanMany = L.scanMany
