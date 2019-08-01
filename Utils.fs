module Utils


let optlistconcat xs ys = 
  match xs with
  | None    -> None
  | Some xs -> 
    match ys with
    | None    -> None
    | Some ys -> Some (xs @ ys)

let ( |@| ) xs ys = optlistconcat xs ys
