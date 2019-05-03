module Eval

open Syntax
open System.Data

(* define of "value" *)
type Exval =
  | IntV of int
  | BoolV of bool
and Dnval = Exval

exception SyntaxErrorException of string

let err s = raise (SyntaxErrorException s)

(* pretty printing *)
let rec stringOfExval = function
    IntV i -> i.ToString()
  | BoolV b -> b.ToString()

let ppVal v = printfn "%s" (stringOfExval v)

(* apply primitive calculation *)
let rec applyPrim op arg1 arg2 =
    match op, arg1, arg2 with
    | Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
    | Plus, _, _ -> err ("Both arguments must be integer: +")
    | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
    | Mult, _, _ -> err ("Both arguments must be integer: *")
    | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
    | Lt, _, _ -> err ("Both arguments must be integer: <")

(* evaluate expression *)
let rec evalExp env = function
    Var x ->
    (try Environment.lookup x env with
       Environment.NotBoundException -> err ("Variable not bound: " + x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | BinOp (op, exp1, exp2) ->
    let arg1 = evalExp env exp1
    let arg2 = evalExp env exp2
    applyPrim op arg1 arg2
  | IfExp (exp1, exp2, exp3) ->
    let test = evalExp env exp1
    (match test with
       BoolV true -> evalExp env exp2
     | BoolV false -> evalExp env exp3
     | _ -> err ("Test expression must be boolean: if"))

(* evaluate declaration *)
let evalDecl env =
    function Exp e ->
                let v = evalExp env e in ("-", env, v)