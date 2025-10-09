{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_llama_hs (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "llama_hs"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "The Llama Programming Language"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/jawadcode/llama-hs"
