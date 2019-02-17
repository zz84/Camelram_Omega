open AST
open BasicOperation
open Derivative

(** [times_cov t1 t2] is the tree that adds [Times] operation on [t1] and [t2], 
    which uses the [t1] as the first tree node and [t2] as the second tree node. 

    Example: 
    [times_cov (Val (Vari "x"))] = [Node (BOperation Times, 
                                          Val (Vari "x"), 
                                          Val (Vari "x"))]
 *)
let times_cov t1 t2 = 
  Node (BOperation Times, t1, t2)

(** [div_cov t1 t2] is the tree that adds [Div] operation on [t1] and [t2], 
    which uses the [t1] as the first tree node and [t2] as the second tree node. 

    Example: 
    [div_cov (Val (Vari "x"))] = [Node (BOperation Div, 
                                        Val (Vari "x"), 
                                        Val (Vari "x"))]
 *)
let div_cov t1 t2 = 
  Node (BOperation Div, t1, t2)

(** [pow_cov t1 t2] is the tree that adds [Power] operation on [t1] and [t2], 
    which uses the [t1] as the first tree node and [t2] as the second tree node. 

    Example: 
    [pow_cov var_x (Val (Float 2.))] = [Node (BOperation Power, 
                                              Val (Vari "x"), 
                                              Val (Float 2.))]
 *)
let pow_cov t1 t2 = 
  Node (BOperation Power, t1, t2)

(** [plus_cov t1 t2] is the tree that adds [Plus] operation on [t1] and [t2], 
    which uses the [t1] as the first tree node and [t2] as the second tree node. 

    Example: 
    [plus_cov (Val (Vari "x"))] = [Node (BOperation Plus, 
                                        Val (Vari "x"), 
                                        Val (Vari "x"))]
 *)
let plus_cov t1 t2 = 
  Node (BOperation Plus, t1, t2)

(** [minus_cov t1 t2] is the tree that adds [Minus] operation on [t1] and [t2], 
    which uses the [t1] as the first tree node and [t2] as the second tree node. 

    Example: 
    [minus_cov (Val (Vari "x"))] = [Node (BOperation Minus, 
                                          Val (Vari "x"), 
                                          Val (Vari "x"))]
 *)
let minus_cov t1 t2 = 
  Node (BOperation Minus, t1, t2)

(** [sin_cov t1] is the tree that adds [Sin] operation on [t1] 
    
    Example: 
    [sin_cov (Val (Vari "x"))] = [Node (UOperation Sin, 
                                        Val Emp, 
                                        Val (Vari "x"))]
*)
let sin_cov t1 = 
  Node (UOperation Sin, Val(Emp), t1)

(** [cos_cov t1] is the tree that adds [Cos] operation on [t1] 
    
    Example: 
    [cos_cov (Val (Vari "x"))] = [Node (UOperation Cos, 
                                        Val Emp, 
                                        Val (Vari "x"))]
*)
let cos_cov t1 = 
  Node (UOperation Cos, Val(Emp), t1)

(** [log_cov t1] is the tree that adds [Log] operation on [t1]. 

    Example: 
    [log_cov (Val (Vari "x"))] = [Node (UOperation Log, 
                                        Val Emp, 
                                        Val (Vari "x"))]
*)
let log_cov t1 = 
  Node (UOperation Log, Val(Emp), t1)

let var_x = Val (Vari "x") 

(** [constant_exp t] checks whether [t] is a constant expression in
    terms of variable "x". 

    Examples: 
    [constant_exp Node (UOperation Log, Val (Vari "y"), Val Emp)] = true
    [constant_exp Node (UOperation Log, Val (Vari "x"), Val Emp)] = false
*)
let rec constant_exp t = 
  match t with
  | Empty -> true
  | Val vl -> 
    (match vl with 
     | Float _ | Const _ | Emp -> true
     | Vari v -> "x"<> v
     | Matrix _ | MatrixDiag _| MatrixSVD _ -> failwith "no matrix operations in integral!"
    ) 
  | Node (op, t1, t2) -> 
    (constant_exp t1) && (constant_exp t2)
  | Par t ->
    constant_exp t

(** [sin_integral t] is the integrated result of [sin t] in terms of [Vari "x"].
    
    Requires: 
    [t] doesn't contain any variables other than [Vari "x"]

    Examples: 
    [sin_integral (Val (Vari "x"))] = [Node (BOperation Times, 
                                            Val (Float (-1.)),
                                            Node (UOperation Cos, 
                                                  Val Emp, 
                                                  Val (Vari "x")))]

    [sin_integral (Node(BOperation Plus, Val (Vari "x"), 
                       (Node (BOperation Power, 
                              Val (Vari "x"), 
                              Val (Float 2.)))))] =  
                    [Node (BOperation Div,
                          Node (UOperation Cos, Val Emp,
                            Node (BOperation Plus, Val (Vari "x"),
                            Node (BOperation Power, Val (Vari "x"), 
                                                    Val (Float 2.)))),
                          Node (BOperation Times, Val (Float (-1.)),
                            Node (BOperation Plus, Val (Float 1.),
                            Node (BOperation Times, Val (Float 1.),
                              Node (BOperation Times, Val (Float 2.),
                              Node (BOperation Power, Val (Vari "x"),
                                Node (BOperation Minus, Val (Float 2.),
                                                        Val (Float 1.))))))))]
*)
let rec sin_integral t = 
  match t with 
  | Empty -> failwith "shouldn't call sin_integral with empty argument"
  | Val v -> 
    (match v with 
     | Float f -> times_cov (Val (Float (sin f))) var_x
     | Const c -> times_cov t var_x
     | Vari x -> 
       if x <> "x" then failwith "no variable other than x is supported"
       else (times_cov (Val (Float (-1.))) (cos_cov (var_x)))
     | _ -> failwith "no matrix in sin")
  | Node (BOperation b, t1, t2) as n -> 
    if constant_exp n then times_cov n var_x 
    else 
      if b <> Power then
        let d = derivative n (Vari "x") in
        let factor = times_cov (Val (Float  (-1.))) d in
        div_cov (cos_cov n) factor
      else failwith "doesn't support this integral"
  | Node (UOperation _, Val (Emp), t2) as n -> 
    if constant_exp n then times_cov n var_x  
    else 
      let d = derivative n (Vari "x") in
      let factor = times_cov (Val (Float (-1.))) d in
      div_cov (cos_cov n) factor
  | _ -> failwith "doesn't support this kind of sin integration"

(** [cos_integral t] is the cos integrated result of [cos t] in 
    terms of [Vari "x"].

    Requires: 
    [t] doesn't contain any variables other than [Vari "x"]

    Examples: 
    [cos_integral (Val (Vari "x"))] =  
                    [Node (UOperation Sin, Val Emp, Val (Vari "x"))];;

    [cos_integral (Node(BOperation Plus, Val (Vari "x"), 
                       (Node (BOperation Power, 
                              Val (Vari "x"), 
                              Val (Float 2.)))))] =  
                    [Node (BOperation Div,
                          Node (UOperation Sin, Val Emp,
                            Node (BOperation Plus, Val (Vari "x"),
                            Node (BOperation Power, Val (Vari "x"), 
                                                    Val (Float 2.)))),
                          Node (BOperation Plus, Val (Float 1.),
                            Node (BOperation Times, Val (Float 1.),
                            Node (BOperation Times, Val (Float 2.),
                              Node (BOperation Power, Val (Vari "x"),
                              Node (BOperation Minus, Val (Float 2.), 
                                                      Val (Float 1.)))))))]
*)
let cos_integral t = 
  match t with 
  | Empty -> failwith "shouldn't call cos_integral with empty argument"
  | Val v -> 
    (match v with 
     | Float f -> times_cov (Val (Float (cos f))) var_x
     | Const c -> times_cov t var_x
     | Vari x -> 
       if x <> "x" then failwith "no variable other than x is supported"
       else sin_cov (var_x)
     | _ -> failwith "no matrix in cos"
    )
  | Node (BOperation _, t1, t2) as n -> 
    if constant_exp n then times_cov n var_x 
    else 
      let d = derivative n (Vari "x") in
      div_cov (times_cov(sin_cov n) (Val (Float (-1.)))) d
  | Node (UOperation _, Val (Emp), t2) as n -> 
    if constant_exp n then times_cov n var_x  
    else 
      let d = derivative n (Vari "x") in
      div_cov (times_cov (Val (Float (-1.))) (sin_cov n)) d
  | _ -> failwith "doesn't support this kind of cos integration"

(** [log_integral t] is the log integrated result of [log t] in 
    terms of [Vari "x"].

    Requires: 
    [t] doesn't contain any variables other than [Vari "x"]

    Examples: 
    [log_integral (Val (Vari "x"))] =  
                    [Node (BOperation Minus,
                          Node (BOperation Times, 
                                Val (Vari "x"),
                                Node (UOperation Log, Val Emp, Val (Vari "x"))),
                          Val (Vari "x"))];;

    [log_integral (Node (BOperation Power, 
                         Val (Vari "x"), 
                         Val (Float 2.)))] = 
                         [Node (BOperation Div,
                              Node (BOperation Times,
                                    Node (BOperation Power, 
                                          Val (Vari "x"), 
                                          Val (Float 2.)),
                                    Node (BOperation Minus,
                                          Node (UOperation Log, 
                                                Val Emp,
                                                Node (BOperation Power, 
                                                      Val (Vari "x"), 
                                                      Val (Float 2.))),
                                          Val (Float 1.))),
                              Node (BOperation Times, 
                                    Val (Float 1.),
                                    Node (BOperation Times, 
                                          Val (Float 2.),
                                          Node (BOperation Power, 
                                                Val (Vari "x"),
                                                Node (BOperation Minus, 
                                                      Val (Float 2.),
                                                      Val (Float 1.)))))) ]

 *)
let log_integral t = 
  match t with 
  | Empty -> failwith "shouldn't call cos_integral with empty argument"
  | Val v -> 
    (match v with 
     | Float f -> times_cov (Val (Float (log f))) var_x
     | Const c -> times_cov t var_x
     | Vari x -> 
       if x <> "x" then failwith "no variable other than x is supported"
       else 
         let p1 = times_cov var_x (log_cov t) in 
         minus_cov p1 var_x
     | _ -> failwith "no matrix in log"
    )
  | Node (BOperation _, t1, t2) as n -> 
    if constant_exp n then times_cov n var_x 
    else 
      let d = derivative n (Vari "x") in
      let inner = minus_cov (log_cov n) (Val (Float 1.)) in
      let deno = times_cov n inner in
      div_cov deno d
  | Node (UOperation _, Val (Emp), t2) as n -> 
    if constant_exp n then times_cov n var_x 
    else 
      failwith "shouldn't have uoperation inside log integral"
  | _ -> failwith "doesn't support this kind of cos integration"

let rec integrate t = 
  match t with 
  | Empty -> failwith "nothing to integrate"
  | Val v -> 
    (match v with 
     | Float f -> times_cov (Val v) var_x
     | Const f -> times_cov (Val v) var_x
     | Vari x -> 
       if x <> "x" then failwith "no variable other than x is supported"
       else times_cov (Val (Float 0.5)) (pow_cov var_x (Val (Float 2.))) 
     | _ -> failwith "no matrix in integral")
  | Node (UOperation op, Val (Emp), t2) -> 
    (match op with 
     | Sin -> sin_integral t2
     | Cos -> cos_integral t2
     | Log -> log_integral t2
     | _ -> failwith "not supported"
    )
  | Node (BOperation op, t1, t2) as n -> 
    if constant_exp n then times_cov n var_x
    else 
      (match op with 
       | Power -> 
         (match t1, t2 with 
          | var_x, Val (Float f) -> 
            let up = pow_cov var_x (Val (Float (f +. 1.))) in
            let deno = plus_cov t2 (Val (Float 1.)) in
            div_cov up deno
          | Val (Const E), _ -> 
            let d = derivative t2 (Vari "x") in
            div_cov n d
          | Val (Float f), _ -> 
            let d = derivative t2 (Vari "x") in
            let ori = div_cov n (log_cov t1) in
            div_cov ori d
          | Val (Const Pi), _ -> 
            let d = derivative t2 (Vari "x") in
            let ori = div_cov n (log_cov (Val (Float (2. *. acos 0.)))) in
            div_cov ori d
          | _ -> failwith "unsupported")
       | Div -> 
         (match t1, t2 with 
          | Val (Float f), Val (Vari ("x")) -> 
            times_cov t1 (log_cov var_x)
          | _ -> failwith "unsupported"
         )
       | Times ->
         begin 
           match t1,t2 with 
           |Val(Float f1),Val(Float f2) -> 
             times_cov (Val(Float (f1 *. f2))) var_x
           |Val(Float f),n |n,Val(Float f) -> 
             times_cov (Val (Float f)) (integrate n)
           |m,n ->
             let d1 = derivative m (Vari "x") in
             let d2 = derivative n (Vari "x") in
             plus_cov(times_cov m d2) (times_cov n d1)
         end 
       | Plus -> 
         plus_cov (integrate t1) (integrate t2)
       | Minus -> 
         minus_cov (integrate t1) (integrate t2)

       |_ -> failwith "unsupported"
      )
  |_ -> failwith "unsupported"


