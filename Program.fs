open System
open FParsec
open Expression
open Evaluate



  


let evalExpression input = 
  match run exprParser input with
  | Failure (str, err, state) -> None
  | Success (v, _, _)         -> 
    v
    |> shuntingYard []
    |> buildTree []
    |> function 
      | Some tree -> eval tree
      | None -> None


let printResult : float option -> unit= 
  function
  | None   -> printfn "Failed evaluating expression"
  | Some v -> 
    if v = Math.Round(v) then
      printfn "%A" (int v)
    else
      printfn "%A" v


let rec repl() = 
  printf "-> "
  let input = Console.ReadLine()
  if input = "q" then
    ()
  else
    evalExpression input
    |> printResult
    repl()


[<EntryPoint>]
let main argv =
  try
    printResult (evalExpression argv.[0])
  with
  | :? IndexOutOfRangeException ->
    printfn "-- Arithmetic expression evaluator --"
    printfn "--       Enter \"q\" to exit       --"
    repl()
  0

