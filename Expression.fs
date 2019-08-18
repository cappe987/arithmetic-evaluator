module Expression

open FParsec
open Utils


type Operator = 
  | Addition
  | Subtraction
  | Multiplication
  | Division
  | Remainder
  | Exponent
  | LeftParens
  | RightParens


type Token = 
  | Operator of Operator
  | Operand  of float


let createOperator s = 
  let inner = 
    function
    | "+"  -> Addition
    | "-"  -> Subtraction
    | "*"  -> Multiplication
    | "/"  -> Division
    | "%"  -> Remainder
    | "**" -> Exponent
    | "("  -> LeftParens
    | ")"  -> RightParens
    | _    -> failwith "Non-supported operator | Should not occur"
  
  Operator (inner s)



let pOperand =
  pfloat |>> Operand 

let pOperator =
  let op = 
    choice (List.map (fun s -> pstring s) ["**"; "+"; "-"; "/"; "*"; "%"; "("; ")"])

  op |>> createOperator

let exprParser : Parser<Token list, unit> = 
  many1 (pOperator <|> pOperand)



// Shunting-Yard Algorithm

let leftAssociative = 
  function
  | Exponent -> false
  | _    -> true


let precedence = 
  function
  | Addition       -> 2
  | Subtraction    -> 2
  | Multiplication -> 3
  | Division       -> 3
  | Remainder      -> 3
  | Exponent       -> 4
  | LeftParens     -> 1
  | RightParens    -> 1 // Should never appear


let shouldPopStack x top =  
  let px   = precedence x
  let ptop = precedence top
  if px < ptop then
    true
  elif px = ptop then
    leftAssociative top
  else
    false


let toToken xs = List.map Operator xs


// type Shunting = {
//   acc    : Token list
//   stack  : Operator list
//   tokens : Token list
// }


let popRightParens stack = 
  let popped = List.takeWhile (fun t -> t <> LeftParens) stack
  let stack  = List.skipWhile (fun t -> t <> LeftParens) stack
  match stack with 
  | []    -> None // No left parenthesis found
  | _::stack ->   // Remove the left from stack
  Some (toToken popped, stack)


let shuntingYard stack = 
  function
  | Operand  x           -> 
    Some ([Operand x], stack) 

  | Operator RightParens -> 
    popRightParens stack

  | Operator LeftParens  ->
    Some ([], LeftParens::stack)

  | Operator x           -> 
    let popped = List.takeWhile (shouldPopStack x) stack
    let stack  = List.skipWhile (shouldPopStack x) stack
    Some (toToken popped, x::stack)


let toPostfix tokens = 
  let rec go acc stack = 
    function
    | [] -> Some (acc @ toToken stack)
    | x::xs -> 
      match shuntingYard stack x with
      | None -> None
      | Some (popped, stack) -> 
        go (acc @ popped) stack xs 

  go [] [] tokens
