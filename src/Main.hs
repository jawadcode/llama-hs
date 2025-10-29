module Main (main) where

import Lexer qualified (readLine, scanMany)
import System.IO (hPutStrLn)

main :: IO ()
main = do
  result <- Lexer.readLine <&> Lexer.scanMany
  case result of
    Right tokens -> map show tokens & unlines & putTextLn
    Left msg -> hPutStrLn stderr msg
