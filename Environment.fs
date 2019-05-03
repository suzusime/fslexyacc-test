module Environment
open System.Xml

type 'a T = (Syntax.Id * 'a) list

exception NotBoundException

let empty = []
let extend x v env = (x,v)::env

let rec lookup x env =
    match List.tryFind (fun y -> let (k, v) = y in k = x) env with
    | Some (k, v) -> v
    | None -> raise NotBoundException

let rec map f = function
    | [] -> []
    | (id, v)::rest -> (id, f v) :: map f rest

let rec foldRight f env a =
    match env with
    | [] -> a
    | (_, v)::rest -> f v (foldRight f rest a)