open AST
open Derivative

(** [calculate x f] is the float value that takes in float [x] into the function
    [f] respresented as AST. [f] is a function with respect to one varible
    Requires:
      [f] is a valid AST representation, and contains only on parameter
      
    Example:
      let [f] = Node(BOperation Minus, 
          Node(BOperation Power, Val(Vari"x"), Val(Float 2.)), Val(Float 3.))
      [calculate 1. f] is [-2.] *)
let calculate x f = 
  let sx = string_of_float x in 
  let res = substitute (f |> format) sx |> parse |> evaluate in 
  (* let res = f |> format |> substitute sx |> parse |> evaluate in  *)
  (* res *)
  match res with 
  | Float(a) -> a
  | Const(Pi) -> 2. *. acos 0.
  | Const(E) -> exp 1.
  | _ -> failwith "not a result here"

(** [rounding a] rounds the float [a] to it's fourth decimal place.
    Example:
      [rounding 1.0000000003455] is [1.]*)
let rounding a = 
  ( /. ) (a*.1e4|> floor) 1e4

let rec newton f init iter = 
  let err = abs_float (calculate init f) in 
  if err < 1e-5 then rounding init
  (* else if iter = 100 then failwith "unable to converge" *)
  else
    let t1 = calculate init f in 
    let t2 = Vari "x" |> derivative f |> calculate init in 
    let new_init = init -. t1/.t2 in 
    newton f new_init (iter + 1)