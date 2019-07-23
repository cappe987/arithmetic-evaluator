module Expression

open Tree
open FParsec

type Token = 
  | Operator of string
  | Operand  of float


let pOperand : Parser<Token, unit>  = 
  pfloat |>> Operand 

let pOperator : Parser<Token, unit> = 
  let op = choice (List.map (fun s -> pstring s) ["**"; "+"; "-"; "/"; "*"; "%"])
  op |>> Operator

let exprParser : Parser<Token list, unit> = 
  many1 (pOperator <|> pOperand)



// Shunting-Yard Algorithm

let leftAssociative = 
  function
  | "**" -> false
  | _    -> true


let precedence = 
  function
  | "+"  -> 2
  | "-"  -> 2
  | "/"  -> 3
  | "*"  -> 3
  | "%"  -> 3
  | "**" -> 4
  | _    -> failwith "Non-supported operator"


let shouldPopStack top x =  
  let px   = precedence x
  let ptop = precedence top
  if px < ptop then
    true
  elif px = ptop then
    if leftAssociative top then true
    else false
  else
    false


let rec popWhile x acc = 
  function
  | []    -> (List.rev acc, [])
  | top::xs -> 
    if shouldPopStack top x then
      popWhile x (top::acc) xs
    else
      (List.rev acc, top::xs)
  

let liftOp xs = List.map Operator xs

let rec shuntingYard (stack : string list)= 
  function
  | []    -> liftOp stack
  | x::xs -> 
    match x with
    | Operand  x -> 
      (Operand x) :: shuntingYard stack xs
    | Operator x ->
      let (popped, stack) = popWhile x [] stack
      (liftOp popped) @ shuntingYard (x::stack) xs



