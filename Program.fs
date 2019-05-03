// Learn more about F# at http://fsharp.org

module Program

open System
open FSharp.Text.Lexing
open Syntax
open Eval

let rec readEvalPrint env =
    printf "# "
    let decl = Parser.toplevel Lexer.main (LexBuffer<char>.FromTextReader stdin)
    let (id, newenv, v) = evalDecl env decl
    printf "val %s = " id;
    ppVal v;
    printfn ""
    readEvalPrint newenv

let initialEnv =
    Environment.empty
    |> Environment.extend "i" (IntV 1)
    |> Environment.extend "v" (IntV 5)
    |> Environment.extend "x" (IntV 10)

[<EntryPoint>]
let main argv =
    readEvalPrint initialEnv
    0 // return an integer exit code
