module Evaluate

open Tree
open Expression



// Construct expression tree from postfix
let rec buildTree stack = 
  function 
  | []                 -> 
    match stack with
    | x::[] -> Some x
    | _ -> None 

  | Operand  x::stream -> 
    buildTree (Leaf (Operand x)::stack) stream

  | Operator x::stream -> 
    match stack with
    | []         -> None 
    | _::[]      -> None 
    | r::l::stack -> 
      let node = root l (Operator x) r
      buildTree (node::stack) stream



let bind2 f x y = 
  match x with
  | Some x -> 
    match y with
    | Some y -> Some (f x y)
    | None -> None
  | None -> None



let getOp =
  function
  | Addition       -> bind2 (+)
  | Subtraction    -> bind2 (-)
  | Multiplication -> bind2 (*)
  | Division       -> bind2 (/)
  | Remainder      -> bind2 (fun x y -> x%y)
  | Exponent       -> bind2 (fun x y -> x**y)


let rec eval = 
  function
  | Leaf v -> 
    match v with
    | Operand x  -> Some x
    | Operator _ -> None

  | Node (v, l, r) -> 
    match v with 
    | Operand _   -> None
    | Operator op -> 
      let f = getOp op
      f (eval l) (eval r)

  | Empty -> None 

