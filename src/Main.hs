module Main (main) where

import Lexer qualified (alexScanTokens, readLine)

main :: IO ()
main = Lexer.readLine <&> Lexer.alexScanTokens >>= print
