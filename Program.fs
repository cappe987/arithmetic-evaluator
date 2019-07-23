open System
open FParsec
open Expression
open Evaluate



  


let evalExpression input = 
  match run exprParser input with
  | Failure (str, err, state) -> None
  | Success (v, _, _)         -> 
    try
      v
      |> shuntingYard []
      |> buildTree []
      |> eval
      |> Some
    with 
      | _ -> None


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
  Console.ReadLine()
  |> evalExpression
  |> printResult
  repl()


[<EntryPoint>]
let main argv =
  try
    printResult (evalExpression argv.[0])
  with
  | :? IndexOutOfRangeException ->
    printfn "-- Arithmetic expression evaluator --"
    repl()
  0

