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

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
