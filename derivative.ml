open AST

(** [belong f x] is the boolean value determining whether the variable
    [x], which is of type value, is in expr t [f]
    Requires:
      [f] is a valid representation of AST
      [x] is of type Vari of string

    Examples:
      [belong Node(BOperation Plus, Val(Float 1.), 
        Node(BOperation Times, Val(Float 2.), Val(Vari "x"))) (Vari "x")]
        is [true]
      [belong Node(UOperation Sin, Val(Const Pi), Val (Emp)) (Vari "x")]
        is [false]*)
let rec belong f x = 
  match f with 
  | Val(a) -> 
    begin
      match a with 
      | Vari(y) as temp -> temp = x
      | _ -> false
    end
  | Node(op, l, r) -> belong l x || belong r x
  | _ -> false

let rec derivative f x = 
  match f with 
  | Empty -> failwith "invalid input"
  | Val(a) -> 
    begin
      match a with
      | Float _ -> Val(Float 0.)
      | Const _ -> Val(Float 0.)
      | Vari(b) as temp -> 
        if temp = x then Val(Float 1.)
        else Val (Float 0.)
      | _ -> failwith "invalid input"
    end
  | Node(op, l, r) as number-> 
    begin
      match op with 
      | BOperation bop ->
        begin
          match bop with
          | Plus ->   Node(BOperation Plus, derivative l x, derivative r x)
          | Minus ->   Node(BOperation Minus, derivative l x, derivative r x)
          | Times -> 
            begin
              let t1 = derivative l x in 
              let t2 = derivative r x in 
              let res1 =   Node(BOperation Times, t1, r) in 
              let res2 =   Node(BOperation Times, l, t2) in 
              Node(BOperation Plus, res1, res2)
            end
          | Div -> 
            begin
              let numerator_t1 =   Node(BOperation Times, r, derivative l x) in 
              let numerator_t2 =   Node(BOperation Times, l, derivative r x) in 
              let denominator =   Node(BOperation Power, r, Val (Float 2.)) in 
              let numerator =   Node(BOperation Minus, numerator_t1, numerator_t2) in 
              Node(BOperation Div, numerator, denominator)
            end
          | Power -> 
            begin
              if (belong l x) && not (belong r x) then 
                let d1 =   Node(BOperation Minus, r, Val (Float 1.)) in 
                let res1 =   Node(BOperation Power, l, d1) in 
                let d2 = derivative l x in 
                let pt1 =   Node(BOperation Times, r, res1) in 
                Node(BOperation Times, d2, pt1)
              else if not (belong l x) && (belong r x) then
                let pt1 =   Node(BOperation Power, l, r) in 
                let pt2 =   Node(UOperation Log, Val(Emp),l) in 
                let t1 =   Node(BOperation Times, pt1, pt2) in 
                let t2 = derivative r x in 
                Node(BOperation Times, t1, t2)
              else failwith "not supported in this calculator"
            end
          | _ -> failwith "invalid derivative input"
        end
      | UOperation uop -> 
        begin
          match uop with 
          | Log -> 
            let t1 = Node(BOperation Div, Val(Float 1.), r) in 
            let t2 = derivative r x in 
            Node(BOperation Times, t1, t2)
          | Sin -> 
            let t1 = derivative r x in 
            let t2 = Node(UOperation Cos, Val(Emp), r) in 
            Node(BOperation Times, t1, t2)
          | Cos -> 
            let t1 = derivative r x in 
            let t2 = Node(UOperation Sin, Val(Emp), r) in 
            let t3 = Node(BOperation Times, Val(Float (-1.)), t2) in 
            Node(BOperation Times, t1, t3)
          | Tan ->
            let sterm = Node(UOperation Sin, Val(Emp), r) in
            let cterm = Node(UOperation Cos, Val(Emp), r) in 
            let f = Node(BOperation Div, sterm, cterm) in 
            derivative f x
          | Exp -> 
            let t1 = derivative r x in 
            Node(BOperation Times, number, t1)
          | _ -> failwith "invalid derivative input"
        end
      | _ -> failwith "node root should always be operator"
    end
  | _ -> failwith "invalid input"



