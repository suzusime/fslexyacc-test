module Syntax

type Id = string

type BinOp = Plus | Mult | Lt

type Exp =
    Var of Id
  | ILit of int
  | BLit of bool
  | BinOp of BinOp
  | IfExp of Exp * Exp * Exp

type Prog =
    Exp of Exp