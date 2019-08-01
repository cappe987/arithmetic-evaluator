open System
open FParsec
open Expression
open Evaluate



  


let evalExpression input = 
  match run exprParser input with
  | Failure (_, _, _) -> None
  | Success (v, _, _) -> 
    v
    |> shuntingYard []
    |> function
      | None    -> None
      | Some xs -> 
        evaluate [] xs
    
    // let x = shuntingYard [] v
    // printfn "%A" x
    // evaluate [] x
    // |> function 
    //   | Some tree -> eval tree
    //   | None -> None


let printResult : float option -> unit = 
  function
  | None   -> printfn "Failed evaluating expression"
  | Some v -> 
    if v = Math.Round(v) then
      printfn "%d" (int v)
    else
      printfn "%f" v


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
  if Array.length argv > 0 then
    printResult (evalExpression argv.[0])
  else
    printfn "-- Arithmetic expression evaluator --"
    printfn "--       Enter \"q\" to exit       --"
    repl()
  0

