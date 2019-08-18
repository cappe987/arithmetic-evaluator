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


//    acc <-----<- input
//      ^         /
//       \       v
//        opstack
type ShuntingYard = {
  acc     : Token list
  opstack : Operator list
  input  : Token list
}


let popRightParens yard = 
  let popped = List.takeWhile (fun t -> t <> LeftParens) yard.opstack
  let stack  = List.skipWhile (fun t -> t <> LeftParens) yard.opstack
  match stack with 
  | []    -> None // No left parenthesis found
  | _::stack ->   // Remove the left from stack
  Some ({yard with acc=yard.acc @ toToken popped; opstack=stack})


let shunting yard = 
  function
  | Operand  x           -> 
    Some {yard with acc=yard.acc @ [Operand x]}

  | Operator RightParens -> 
    popRightParens yard

  | Operator LeftParens  ->
    Some {yard with opstack=LeftParens::yard.opstack}

  | Operator x           -> 
    let popped = List.takeWhile (shouldPopStack x) yard.opstack
    let stack  = List.skipWhile (shouldPopStack x) yard.opstack
    Some {yard with acc=yard.acc @ toToken popped; opstack=x::stack}


let ( >>= ) a f = Option.bind f a

let toPostfix input = 
  let rec go yard = 
    match yard.input with
    | [] -> Some (yard.acc @ toToken yard.opstack) 
    | x::xs -> 
      let yard = {yard with input=xs}
      shunting yard x >>= (fun yard -> go yard)

  go {acc=[]; opstack=[]; input=input}
