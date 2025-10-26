{-# LANGUAGE TypeFamilies #-}

module AST (Pass) where

data Pass = Parsed | Renamed | Typed

data Exp (p :: Pass)
  = ELit (XLit p) Lit
  | EVar (Var p)
  | ELam (XLam p) (Var p) (Exp p)

data Var (p :: Pass) = Var (XVar p) Text

type family XVar (p :: Pass)

type family XLit (p :: Pass)

data Lit
  = LBool Bool
  | LInt Int
  | LFloat Float
  | LString Text

type family XLam (p :: Pass)
