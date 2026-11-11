module AST () where

import Relude

data Expr
    = EIdent Interned
    | ELitUnit
    | ELitString Str

data Interned = Name {unIntern :: !Text, internedId :: !Int}

newtype Ident = Ident Interned

newtype Str = Str Interned

instance Eq Interned where
    n1 == n2 = internedId n1 == internedId n2

instance Ord Interned where
    compare n1 n2 = compare (internedId n1) (internedId n2)
