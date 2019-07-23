module Evaluate

open Tree
open Expression



let rec buildTree stack = 
  function 
  | []                 -> 
    match stack with
    | x::[] -> x
    | _ -> failwith "Error @ buildTree"

  | Operand  x::stream -> 
    buildTree (Leaf (Operand x)::stack) stream

  | Operator x::stream -> 
    match stack with
    | []         -> emptyTree //
    | _::[]      -> failwith "Error @ buildTree | Invalid operator to operand ratio"
    | r::l::stack -> 
      let node = root l (Operator x) r
      buildTree (node::stack) stream


let getOp =
  function
  | "+"  -> (+)
  | "-"  -> (-)
  | "/"  -> (/)
  | "*"  -> (*)
  | "%"  -> fun x y -> x%y
  | "**" -> fun x y -> x**y
  | _    -> failwith "Non-supported operator"

let rec eval = 
  function
  | Leaf v -> 
    match v with
    | Operator x   -> failwith "Error @ eval | Operator can't be leaf"
    | Operand  v   -> v

  | Node (v, l, r) -> 
    match v with
    | Operand  v -> failwith "Error @ eval | Found value, expected operator"
    | Operator x ->
      let f = getOp x
      f (eval l) (eval r)

  | Empty -> failwith "Error @ eval | Expected something, got nothing"


