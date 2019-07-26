module Expression

open FParsec


type Operator = 
  | Addition
  | Subtraction
  | Multiplication
  | Division
  | Remainder
  | Exponent

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
    | _    -> failwith "Non-supported operator | Should not occur"
  
  Operator (inner s)



let pOperand =
  pfloat |>> Operand 

let pOperator =
  let op = choice (List.map (fun s -> pstring s) ["**"; "+"; "-"; "/"; "*"; "%"])
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


let shouldPopStack x top =  
  let px   = precedence x
  let ptop = precedence top
  if px < ptop then
    true
  elif px = ptop then
    leftAssociative top
  else
    false


let liftOp xs = List.map Operator xs

let rec shuntingYard stack = 
  function
  | []    -> liftOp stack // Retrieve the remaining operators
  | x::xs -> 
    match x with
    | Operand  x -> 
      (Operand x) :: shuntingYard stack xs
    | Operator x ->
      let popped = List.takeWhile (shouldPopStack x) stack
      let stack  = List.skipWhile (shouldPopStack x) stack
      (liftOp popped) @ shuntingYard (x::stack) xs



