{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Span where

import Relude

data Span = Span { start :: Int, end :: Int }

instance Semigroup Span where
  ( <> ) a b = Span { start = start a, end = end b }
