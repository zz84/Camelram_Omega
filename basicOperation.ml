let plus a b = 
  a +. b 

let minus a b = 
  a -. b

let times a b = 
  a *. b

let divide a b = 
  a /. b

let power base power = 
  base ** power

let remainder a b = 
  let a' = int_of_float a in 
  let b' = int_of_float b in 
  if (float_of_int a' = a && float_of_int b' = b) then float_of_int ((mod) a' b')
  else failwith "remainder should be called on integer number"

let negative a = 
  minus 0.0 a 

let log a =
  log a  

let exp a = 
  exp a
  
let sin a = 
  sin a

let cos a = 
  cos a

let tan a = 
  tan a

let asin a = 
  asin a

let acos a = 
  acos a

let atan a = 
  atan a

(** [frac a] is the fractorial result of [a], multiplying [a] 
    with all integers that are less than [a] but greater than 0. 
    Requires: [a] is non-negative integers represented in float *)
let frac a = 
  let rec helper acc a = 
    match a with 
    | 0. -> acc
    | n -> helper (acc *. a) (a -. 1.)
  in if a < 0. then failwith "negative fractorial" 
  else (helper 1.0 a)

let  permutation tot sub = 
  let rec helper acc t= 
    if t = tot -. sub then acc 
    else helper (acc*. t) (t -.1.)
  in
  if tot < 0. || sub < 0. then failwith "negative tot or sub"
  else if tot < sub then failwith "tot less than sub " 
  else helper 1. tot

let combination tot sub = 
  let permu = permutation tot sub in 
  let sub_frac = frac sub in 
  permu /. sub_frac
