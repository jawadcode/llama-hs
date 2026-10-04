module Main (main) where

import Lexer qualified (scanMany)
import Relude
import System.IO (hPutStrLn)

main :: IO ()
main = do
  result <- getLine <&> encodeUtf8 <&> Lexer.scanMany
  case result of
    Right tokens -> map show tokens & unlines & putTextLn
    Left msg -> hPutStrLn stderr msg
