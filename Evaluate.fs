module Evaluate

open Expression


let getOperation =
  function
  | Addition       -> (+)
  | Subtraction    -> (-)
  | Multiplication -> (*)
  | Division       -> (/)
  | Remainder      -> (fun x y -> x%y)
  | Exponent       -> (fun x y -> x**y)
  | LeftParens     -> failwith "Left not popped"
  | RightParens    -> failwith "Right not popped"


let evalSingle op left right= 
  let f = getOperation op
  f left right

// Evaluate postfix expression
// Same algorithm as building a tree from postfix, but evaluated immediately
let evaluate tokens = 
  let rec eval stack = 
    function 
    | []                 -> 
      match stack with
      | x::[] -> Some x
      | _ -> None 

    | Operand  x::stream -> 
      eval (x::stack) stream

    | Operator x::stream -> 
      match stack with
      | r::l::stack -> 
        let value = evalSingle x l r
        eval (value::stack) stream

      | _ -> None
    
  eval [] tokens
