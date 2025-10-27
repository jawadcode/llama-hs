module Main (main) where

import Lexer

main :: IO ()
main = print =<< runLex
